# === IMPORTS ===
import requests
import json
import logging
import mimetypes
import os
from bs4 import BeautifulSoup


import vertexai
from vertexai.language_models import TextEmbeddingModel, TextEmbeddingInput
from vertexai.vision_models import ImageCaptioningModel
from google import genai
from google.cloud import firestore
from google.cloud import aiplatform
from google.cloud.firestore_v1.vector import Vector
from google.generativeai import GenerativeModel
from google.genai import types
from google.genai.types import (
    CreateBatchJobConfig,
    EmbedContentConfig,
    HttpOptions,
)



# === CONFIGURATION ===
PROJECT_ID = "..."
LOCATION = "..."
BASE_URL = "..."
API_BASE = f"{BASE_URL}/rest/api/"
HEADERS = {
     "Authorization": "Bearer ...",
     "Accept": "application/json"
}


# === INITIALISATION DES CLIENTS ===
vertexai.init(project=PROJECT_ID, location=LOCATION)
client = genai.Client(project=PROJECT_ID, location=LOCATION, vertexai=True, http_options=HttpOptions(api_version="v1"))
firestore_client = firestore.Client(project=PROJECT_ID, database="confluence")



# === FONCTIONS D'INTERACTION AVEC CONFLUENCE ===


def get_all_spaces():
    """Récupère tous les espaces Confluence via l'API."""
    all_spaces = []
    start, limit = 0, 24


    while True:
        params = {"start": start, "limit": limit}
        response = requests.get(f"{API_BASE}space", headers=HEADERS, params=params)
        if response.status_code != 200:
            print(f"Erreur récupération espaces : {response.status_code} - {response.text}")
            break


        data = response.json()
        all_spaces.extend(data.get("results", []))


        if not data.get("_links", {}).get("next"):
            break
        start += limit


    return all_spaces



def get_SDV(spaces):
    """Retourne l'espace  s'il existe."""
    return next((s for s in spaces if s['key'] == '...'), "Error 404 : Not Found")



def get_pages_for_space(space_key):
    """Récupère toutes les pages d'un espace donné."""
    all_pages = []
    start, limit = 0, 156


    while True:
        params = {
            "start": start, "limit": limit,
            "spaceKey": space_key, "type": "page", "expand": "_links"
        }
        response = requests.get(f"{API_BASE}content", headers=HEADERS, params=params)
        if response.status_code != 200:
            print(f"Erreur récupération pages : {response.status_code} - {response.text}")
            break


        data = response.json()
        pages = data.get("results", [])
        print(f"{len(pages)} pages récupérées à start={start}")
        all_pages.extend(pages)


        if len(pages) < limit:
            break
        start += limit


    return all_pages




# === TRAITEMENT DU CONTENU DES PAGES ===



def describe_non_textual_element(filename : str, download_url : str) -> str:
    response = requests.get(download_url, headers=HEADERS)
    if response.status_code != 200:
        print(f"Erreur téléchargement de {filename} : {response.status_code}")
        retir, f"[Element non textuel : {filename}]"


    os.makedirs("/tmp/confluence_attachments/", exist_ok=True)
    file_path = f"/tmp/confluence_attachments/{filename}"
    with open(file_path, "wb") as f:
        f.write(response.content)


    if filename.lower().endswith(('.png', '.jpg', ".jpeg")):
        with open(file_path, "rb") as f:
            image_data = f.read()
        response = client.models.generate_content(
            model="gemini-2.0-flash-001",
            contents=[
            types.Part.from_bytes(
                data=image_data,
                mime_type='image/jpeg',
            ),
            'Caption this image.'
            ]
        )
        return response.text
    
    elif filename.lower().endswith(('.mp4', '.mov')):
        with open(file_path, "rb") as f:
            video_data = f.read()
        response = client.models.generate_content(
        model="gemini-2.0-flash-001",
        contents=[
        types.Part.from_bytes(
                data=video_data,
                mime_type='video/mp4',
            ),
            'Caption this video.'
            ]
        )
        return response.text
    
    return f"[Element non textuel non pris en charge : {filename}]"




def get_page_content(page_id):
    """Récupère et nettoie le contenu HTML d'une page Confluence."""
    url = f"{API_BASE}content/{page_id}"
    params = {"expand": "body.storage,version"}
    response = requests.get(url, headers=HEADERS, params=params)


    if response.status_code != 200:
        print(f"Erreur récupération contenu page {page_id}: {response.status_code}")
        return "", ""


    data = response.json()
    last_updated = data.get("version", {}).get("when", None)
    html = data.get("body", {}).get("storage", {}).get("value", "")
    if not html:
        print(f"Aucun contenu HTML pour la page {page_id}")
        return "", ""


    soup = BeautifulSoup(html, "html.parser")


    # Suppression des balises non pertinentes
    for tag in soup(["nav", "footer", "header", "aside", "script", "style", "link", "meta", "form"]):
        tag.decompose()


    # Extraction du texte utile
    useful_tags = ["h1", "h2", "h3", "h4", "h5", "h6", "p", "ul", "ol", "li", "table", "tr", "td", "th"]
    cleaned_html = BeautifulSoup("", "html.parser")
    for tag in soup.find_all(useful_tags):
        cleaned_html.append(tag)


    text = cleaned_html.get_text(separator="\n\n").strip()


    # Ajout des descriptions des pièces jointes non textuelles
    non_textual_explications = []
    attachments_url = f"{API_BASE}content/{page_id}/child/attachment"
    attachments_response = requests.get(attachments_url, headers=HEADERS)


    if attachments_response.status_code == 200:
        attachments = attachments_response.json().get("results", [])
        for attachment in attachments:
            filename = attachment.get("title", "")
            download_link = attachment["_links"].get("download")
            if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.mp4', '.mov')):
                download_url = f"{BASE_URL}{download_link}"
                try:
                    explication = describe_non_textual_element(filename, download_url)
                    non_textual_explications.append(f"[Explication d'image/vidéo : {explication}]")
                except Exception as e:
                    print(f"Erreur Vertex AI : {e}")
                    non_textual_explications.append(f"[Element non textuel : {filename}]")
    else:
        print(f"Erreur récupération des pièces jointes : {attachments_response.status_code}")
    
    if non_textual_explications:
        text += "\n\n" + "\n".join(non_textual_explications)


    if not text.strip():
        print(f"La page {page_id} est vide.")
        return "", ""


    return text, last_updated



def vectorize_chunks(text):
    """Génère un vecteur d'embedding pour un texte donné."""
    response = client.models.embed_content(
        model="text-embedding-004",
        contents=[text],
        config=EmbedContentConfig(output_dimensionality=768)
    )
    return response.embeddings[0].values


def summarize_text(text):
    """Génère un résumé pour un texte donné."""
    response = client.models.generate_content(
        model="gemini-2.0-flash-001",
        contents=[text, "Summarize this text"],
    )
    return response.text



# === PROGRAMME PRINCIPAL ===


print("Début du traitement...")


spaces = get_all_spaces()
SDV = get_SDV(spaces)
pages = get_pages_for_space(SDV['key'])
collection = firestore_client.collection("pages_oups")


for page in reversed(pages):
    page_ref = collection.document(page['id'])
    content, last_update = get_page_content(page['id'])
    true_content = f"Titre : {page['title']}, \n Contenu : {content}"
    if content:
        nouveau_titre = page['title'].replace(" ", "+")
        if len(content.split()) > 500:
            summary = summarize_text(true_content)
            embedding = vectorize_chunks(summary)
            page_data = {
                "page_id": page['id'],
                "page_title": page['title'],
                "page_url": "..." + nouveau_titre,
                "page_content": true_content,
                "embedding_field": Vector(embedding),
                "page_summary" : summary,
                "page_last_update" : last_update,
            }
        else:
            embedding = vectorize_chunks(true_content)
            page_data = {
                "page_id": page['id'],
                "page_title": page['title'],
                "page_url": "..." + nouveau_titre,
                "page_content": true_content,
                "embedding_field": Vector(embedding),
                "page_last_update": last_update,
            }
        collection.document(page['id']).set(page_data)
    else:
        print(f"Erreur d'embedding pour la page {page['id']}")