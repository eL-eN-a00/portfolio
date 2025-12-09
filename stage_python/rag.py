# === IMPORTS ===
import logging
import streamlit as st
import vertexai
from google import genai
from google.cloud import firestore
from google.cloud.firestore_v1.vector import Vector
from google.cloud.firestore_v1.base_vector_query import DistanceMeasure
from google.genai.types import EmbedContentConfig, HttpOptions
from google.generativeai import GenerativeModel


# === CONFIGURATION ===
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
collection = firestore_client.collection("pages")



# === FONCTIONS DE RAG ===


def get_similar_pages(prompt: str, filters: list = [], num_docs: int = 5, embedding_model_str: str = "text-embedding-005") -> tuple:
    """
    Recherche les pages les plus similaires à un prompt donné via embeddings.


    Args:
        prompt (str): Question utilisateur.
        filters (list): Filtres Firestore (ex: droits d'accès).
        num_docs (int): Nombre de documents à retourner.
        embedding_model_str (str): Modèle d'embedding à utiliser.


    Returns:
        tuple: (pages, titres formatés)
    """
    filtered_pages = collection
    for filter_ in filters:
        filtered_pages = filtered_pages.where(filter=FieldFilter(*filter_))


    filtered_pages = filtered_pages.select([
        "page_id", "page_title", "page_url", "page_content", "embedding_field"
    ])


    response = client.models.embed_content(
        model="text-embedding-004",
        contents=[prompt],
        config=EmbedContentConfig(output_dimensionality=768)
    )
    embedded_prompt = response.embeddings[0].values


    vector_query = filtered_pages.find_nearest(
        vector_field="embedding_field",
        query_vector=Vector(embedded_prompt),
        limit=num_docs,
        distance_measure=DistanceMeasure.COSINE
    )


    topn_pages = [page.to_dict() for page in vector_query.stream()]
    for page in topn_pages:
        del page["embedding_field"]


    retrieved_docs = [f"{page['page_title']} ({page['page_url']})" for page in topn_pages]
    logging.debug(f"Retrieved docs: {retrieved_docs}")


    return topn_pages, retrieved_docs



def construire_prompt_augmente(pages_pertinentes, question_utilisateur, instructions=None):
    """
    Construit un prompt structuré pour une LLM à partir de documents.


    Args:
        pages_pertinentes (list[str]): Contenus des pages.
        question_utilisateur (str): Question posée.
        instructions (str, optional): Instructions pour la LLM.


    Returns:
        str: Prompt complet.
    """
    if instructions is None:
        instructions = (
            "Réponds à la question suivante en te basant uniquement sur les pages fournies. "
            "Si la réponse n'est pas dans les propositions, indique que tu ne sais pas. "
            "Cite tes sources si possible."
        )


    contexte_formate = "\n\n".join(
        f"[Proposition {url}]\n{content}\n" for (content, url) in pages_pertinentes
    )


    prompt = (
        f"{instructions}\n\n"
        f"{contexte_formate}\n\n"
        f"[Question de l'utilisateur]\n{question_utilisateur}"
    )


    return prompt



def generer_reponse_avec_gemini(prompt):
    """
    Génère une réponse à partir d'un prompt avec Gemini.


    Args:
        prompt (str): Prompt structuré.


    Returns:
        str: Réponse générée.
    """
    response = client.models.generate_content(
        model="gemini-2.0-flash-001",
        contents=prompt
    )
    return response.text



# === INTERFACE STREAMLIT ===


st.set_page_config(
    layout="wide",
    page_icon="https://img.icons8.com/ios-filled/renault.png",
    page_title="Agent SDV"
)


if "RAG" not in st.session_state:
    st.session_state["RAG"] = ""


st.subheader("Ask your question about SDV")


context_content_text = st.text_area(
    "What is your question ?",
    key="requirement_defined",
    placeholder="You can add here your question...\n\n\n",
    height=600,
)


generate_doc = st.button("Generate Answer", key="generate_doc")


if generate_doc:
    with st.spinner("Generating your answer ..."):
        question = context_content_text
        pages, docs = get_similar_pages(prompt=question, filters=[], num_docs=5)
        prompt = construire_prompt_augmente(
            pages_pertinentes=[(p["page_content"], p["page_url"]) for p in pages],
            question_utilisateur=question
        )
        reponse = generer_reponse_avec_gemini(prompt)


        # Formatage de la réponse
        response_req2uc = (
            "\n\n**Réponse générée :**\n" +
            reponse
        )


        st.session_state["RAG"] = response_req2uc


st.markdown(st.session_state["RAG"])