import streamlit as st
import pandas as pd
from sparql_processor import run_sparql_query
from streamlit_agraph import Node, Edge, Config, agraph

st.title("Film Finder")
film_name = st.text_input("Enter the name of a film")

if st.button("Search") or film_name:
    ask_results = run_sparql_query(sparql_param=film_name,
                                   sparql_file="./sparql_queries/film_title_ask.sparql")
    
    if ask_results["boolean"]:

        results = run_sparql_query(sparql_param=film_name,
                                   sparql_file="./sparql_queries/film_details_with_number.sparql")
        if results["results"]["bindings"]:
            columns = ["title", "runtime", "directorLabel", "countryLabel"]

            table_data = []
            for result in results["results"]["bindings"]:
                row_data = {}
                for column in columns:
                    if result.get(column):
                        row_data[column] = result[column]["value"]
                    else:
                        row_data[column] = "-"
                table_data.append(row_data)
            table_data = pd.DataFrame(table_data)
            st.dataframe(table_data)

            film_number = results["results"]["bindings"][0]["filmNumber"]["value"]
            st.write("Number of film results: " + film_number)

        graph = run_sparql_query(sparql_param=film_name, sparql_file="./sparql_queries/film_construct.sparql")

        nodes = {}
        edges = []

        for binding in graph["results"]["bindings"]:
            s = binding["s"]["value"]
            p = binding["p"]["value"]
            o = binding["o"]["value"]

            if s not in nodes:
                subject_node = Node(id=s, label=s)
                nodes[s] = subject_node

            if o not in nodes:
                object_node = Node(id=o, label=o)
                nodes[o] = object_node

            edge = Edge(source=s, target=o, label=p)
            edges.append(edge)

        config = Config(width=750, height=950, directed=True, physics=True, hierarchical=False)
        agraph(nodes=list(nodes.values()), edges=edges, config=config)

    else:
        st.write("No results found for the entered film name.")