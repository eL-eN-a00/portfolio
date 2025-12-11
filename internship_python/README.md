# Internship Project - Intelligent RAG Agent for Technical Documentation

## Overview

This project was developed during my **8-week internship at Ampère Software Technology (Renault Group)** as part of my integrated preparatoy program at **Toulouse INP**.

The objective was to design and prototype an **intelligent Retrieval-Augmented Generation (RAG) agent** capable of answering technical questions using internal documentation in the **Software Defined Vehicle (SDV)** domain.

## Project Scope

The work covered the end-to-end construction of a functional RAG system, including:
- **Data extraction and structuration** from Confluence
- **Vectorisation and storage** using **Google Firestore** and **Vertex AI**.
- **Development of several agent versions**, progressing from a simple retrieval script to a contextual conversational assitant capable of:
    - natural, interactive dialogue
    - grounded answers with **source citations**
    - adapting responses to domain-specific constraints
    - serving a prototype interface built with Streamlit

## Authentication and Access Control

A critical component of this project was implementing **OAuth2 authentication**.

The agent verifies the identity of the connected user and **restricts retrieval to documentation the user is authorized to access**.

This ensures that:
- the RAG system never exposes protected content,
- the assistant responds strictly within the user's permission scope,
- compliance with the company's internal access policies is maintained.

This feature required integrated authentication, permission checking, and secure filtering directly into the retrieval pipeline.

## Repository Structure

- donnees.py: 
Implements data ingestion, preprocessing, structuration, and vectorization for the RAG knowledge base.
- RAG.py:
Contains the retrieval and generation pipeline: embeddings, similarity search, context assembly, and answer generation.
- chatdroits.py: 
It's RAG but better -> manages the conversational flow, user session handling, OAuth2 authentication, and **access-level filtering** ensuring that results are scoped to the user's permissions.

## Important Notes

- **The code will not run in a standard environment**. It relies on **paid Google Cloud Services** (Firestore, Vertex AI, embedding models).
- **All sensitive or proprietary information has been removed** to comply with confidentiality obligations.
- The repository is intended as a **technical demonstration** of the system's architecture and logic, not as a deployable product solution.