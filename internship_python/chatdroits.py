# === IMPORTS ===
import logging
import html
import streamlit as st
import pandas as pd
import time
import asyncio
import os
import vertexai
import requests
from google import genai
from google.cloud import firestore
from google.cloud.firestore_v1.vector import Vector
from google.cloud.firestore_v1.base_vector_query import DistanceMeasure
from google.genai import types
from google.genai.types import(
    EmbedContentConfig,
    HttpOptions,
    GenerateContentResponse,
    GenerateContentConfig,
    Content, Part,
)
from google.generativeai import GenerativeModel
from google.adk.agents import LlmAgent, Agent
from google.adk.sessions import InMemorySessionService
from google.adk.runners import Runner, Event
from requests_oauthlib import OAuth2Session


# === CONFIGURATION OAUTH CONFLUENCE ===
CLIENT_ID = "..."
CLIENT_SECRET = "..."
REDIRECT_URI = "..."
AUTHORIZATION_BASE_URL = "..."
TOKEN_URL = "..."
SCOPES = ["READ"]


# === CONFIGURATION ===
os.environ['GOOGLE_GENAI_USE_VERTEXAI'] = 'True'
os.environ['GOOGLE_CLOUD_PROJECT'] = '...'
os.environ['GOOGLE_CLOUD_LOCATION'] = '...'


PROJECT_ID = "..."
LOCATION = "..."


vertexai.init(project=PROJECT_ID, location=LOCATION)
client = genai.Client(
    project=PROJECT_ID,
    location=LOCATION,
    vertexai=True,
    http_options=HttpOptions(api_version="v1")
)
firestore_client = firestore.Client(project=PROJECT_ID, database="confluence")
collection=firestore_client.collection("pages_oups")



# === FONCTIONS DE RAG ===


def get_similar_pages(prompt : str, oauth_token: dict, filters: list = [], num_docs: int = 5) -> list:
    """Search for the most similar Confluence pages from a given prompt.


    Args:
        prompt (str): user's input
        filters (list): Firestore filters (empty for now)
        num_docs (int): Maximum number of docs to return


    Returns:
        topn_pages: list of the pertinent pages
    
    """
    oauth = OAuth2Session(CLIENT_ID, token=oauth_token)
    filtered_pages = collection
    for filter_ in filters:
        filtered_pages = filtered_pages.where(filter=FieldFilter(*filter_))


    filtered_pages = filtered_pages.select([
        "page_title", "page_url", "page_content", "page_last_update","embedding_field"
    ])


    response = client.models.embed_content(
        model="text-embedding-004",
        contents=[prompt],
        config=EmbedContentConfig(output_dimensionality=768),
    )
    embedded_prompt = response.embeddings[0].values


    vector_query = filtered_pages.find_nearest(
        vector_field="embedding_field",
        query_vector=Vector(embedded_prompt),
        limit=num_docs,
        distance_measure=DistanceMeasure.COSINE,
        distance_threshold=0.5,
    )


    topn_pages = []
    for page in vector_query.stream():
        page_dict = page.to_dict()
        page_id = page_dict.get("page_id")
        page_url = page_dict.get("page_url")
        try:
            response = oauth.get(page_url)
            if response.status_code == 200:
                del page_dict["embedding_field"]
                topn_pages.append(page_dict)
        except requests.RequestException:
            continue
    
    if not topn_pages:
        return [{"error": "You don't have access to any of the pages regarding this information or they are inaccessible."}]


    return topn_pages


def get_similar_pages_tool(prompt: str):
    return get_similar_pages(prompt, st.session_state.oauth_token)



# === INTERFACE STREAMLIT ===



requirement_prompt = """


## Role
You are a **senior software engineer** with deep expertise in the **automotive industry**, specifically in the domain of **Software-Defined Vehicles (SDVs)**. 
Your knowledge is grounded in the **Confluence knowledge base** used by SDV engineering teams.


---


### Input
When a fellow engineer submits **a technical requirement question**, your task is to:


1. **Call your tool 'get_similar_pages'** using the user's input as prompt.
2. Carefully read the returned pages
    there should be information in the content, but also in the title and URL.
3. **Only accessible pages are returned**: if a page is not accessible (e.g due to permission issues), it is automatically excluded.
4. If no accessible pages are found, inform the user: _"You do not have access to any relevant pages or they are currently unavailable."_


Based **ONLY on the content of the accessible pages**, write a response that:
    - **Clearly and accurately addresses the user's question**
    - **Give the exact Confluence page(s) URL** used (this is mandatory) (it should begin with https://confluence....)
    - Is technically sound, concise and written in a professional tone
    - look for the last update of the page, and if it was a long time ago, warn the user that the information may not be up to date 
---


##  If the requirement is not directly related to SDVs:


- Still check the results from `get_similar_pages`, as they may contain relevant context. SDV is a broad subject that contains many various domains.
- If no relevant SDV content is found:
 - Politely inform the user that the request falls outside the SDV scope.
 - Suggest how the topic could be reframed to align with SDV concerns.
 - Optionally recommend related SDV topics or documentation that might be helpful.



### Reward
A correct, clear, and well-sourced answer earns you **€10,000**.


### Example of a good answer


**Question :** 'what does SOT mean?'


**Answer : ** 'SOT stands for **Source Of Truth** (...).'
    """



st.set_page_config(
    layout="wide",
    page_icon="https://img.icons8.com/ios-filled/renault.png",
    page_title="Agent SDV"
)



async def generate_response(parts):
    session_id = "SDV_agent_ecg"
    app_name = "Agent SDV "
    user_id = "..."
    if "session_service" not in st.session_state:
        session_service = InMemorySessionService()
        st.session_state.session_service = await session_service.create_session(app_name=app_name, user_id=user_id, session_id=session_id)
    if "runner" not in st.session_state:
        st.session_state.runner = Runner(agent=st.session_state.llm_agent, app_name=app_name, session_service=session_service)


    user_message = Content(role="user", parts=parts)


    response = ""
    async for event in st.session_state.runner.run_async(new_message=user_message, user_id=user_id, session_id=session_id):
        if isinstance(event.content.parts[0].text, str):
            response += event.content.parts[0].text
    return response


def authenticate_user():
    oauth = OAuth2Session(CLIENT_ID, redirect_uri=REDIRECT_URI, scope=SCOPES)
    authorization_url, state = oauth.authorization_url(AUTHORIZATION_BASE_URL)
    st.session_state.oauth_state = state
    st.markdown(f"Clique ici pour te connecter à Confluence :  {authorization_url}")
    st.success("Connecté à Confluence")
    



st.header("Agent SDV")


st.title("Authentification Confluence")


logging.debug("Checking headers...")
headers = st.context.headers
logging.debug(f"---> Headers: {headers}")
if headers:
    email_user = headers.get("X-Goog-Authenticated-User-Email")
    email = email_user.split(":")[1] if email_user else None
    logging.debug(f"Found email: {email}")
else:
    logging.error("HOP HOP HOP, je sais pas d'où tu viens CABRON mais t'as pas le droit d'être là é_é")


if email:
    st.write(f"Connecté en tant que {email}")


if "confluence_token" not in st.session_state:
    authenticate_user()



query_params = st.query_params
if "code" in query_params and "oauth_token" not in st.session_state:
    code = query_params["code"]
    token_response = requests.post(
        TOKEN_URL,
        data = {
            "grant_type" : "authorization_code",
            "code": code,
            "redirect_uri": REDIRECT_URI,
            "client_id": CLIENT_ID,
            "client_secret": CLIENT_SECRET,
        },
        headers={"Accept":"application/json"}
    )


    if token_response.ok:
        token = token_response.json()
    st.session_state.oauth_token = token
    st.success("Authentification réussie !")


if "chat_started_for_use_case" not in st.session_state:
    st.session_state.chat_started_for_use_case = False
if "tc_gen_messages_chat_for_use_case" not in st.session_state:
    st.session_state.tc_gen_messages_chat_for_use_case = []
if "llm_agent" not in st.session_state:
    st.session_state.llm_agent = LlmAgent(
    name="sdv_agent",
    model="gemini-2.0-flash-001",
    instruction=requirement_prompt,
    tools=[get_similar_pages_tool]
)



for message in st.session_state.tc_gen_messages_chat_for_use_case:
    with st.chat_message(message["role"]):
        if message["role"] == "user":
            st.markdown(message["content"])
        else:
            st.write(message["content"])
if prompt_ctc := st.chat_input(
    "Chat with Agent SDV",
    accept_file="multiple",
    file_type=["jpg", "jpeg", "png", "txt", "csv", "pdf", "xls", "xlsm", "xlsx", "xlsb", "yaml"],
    ):
    user_msg_txt = str(prompt_ctc["text"])
    with st.chat_message("user"):
        st.markdown(user_msg_txt)
    if prompt_ctc["files"]:
        for file in prompt_ctc["files"]:
            if any(substring in file.type for substring in ["png", "jpg", "jpeg"]):
                st.image(file)
            else:
                st.caption(file.name)
    
    response = None
    file_user = None


    with st.spinner(f"Generating your answer..."):
        parts = [Part(text=user_msg_txt)]


        if prompt_ctc["files"]:
            for file_user in prompt_ctc["files"]:
                if any(substring in file_user.type for substring in ["png", "jpg", "jpeg", "pdf"]):
                    image_part = types.Part.from_bytes(mime_type=file_user.type, data=file_user.read())
                    parts.append(image_part)
                    file_user.seek(0)
                
                elif any(substring in file_user.type for substring in ["csv", "text", "octet-stream"]):
                    try:
                        list_lines_dataframe_csv = [line.decode("utf-8") for line in file_user.readlines()]
                    except UnicodeDecodeError:
                        file_user.seek(0)
                        list_lines_dataframe_csv = [line.decode("latin-1") for line in file_user.readlines()]
                    str_dataframe_csv = "\n".join(list_lines_dataframe_csv)
                    csv_text_part = types.Part.from_text(text=str_dataframe_csv)
                    parts.append(csv_text_part)
                    file_user.seek(0)
                
                elif any(substring in file_user.type for substring in ["excel"]):
                    excel_text = pd.read_excel(file_user).to_csv(index=False)
                    excel_part = types.Part.from_text(text=excel_text)
                    parts.append(excel_part)
                    file_user.seek(0)
                
                else:
                    st.error("Please select a correct format (.jpg, .jpeg, .png, .txt, .csv, .pdf, .yaml, excel files)")


        response = asyncio.run(generate_response(parts))
                
        if response:
            st.session_state.tc_gen_messages_chat_for_use_case.extend(
                [
                    {"role": "user", "content": prompt_ctc["text"]},
                    {"role": "assistant", "content": response}
                ]
            )
            with st.chat_message("assistant"):
                st.write(response)
