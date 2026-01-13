import json

from rdflib import RDF, RDFS, SDO, XSD, Graph, Literal, Namespace

# copy from the rdfs file
EXA = Namespace("http://example.org/abox#")
EXT = Namespace("http://example.org/tbox#")
DBO = Namespace("http://dbpedia.org/ontology/")


def create_rdf_from_json(json_data):
    g = Graph()

    g.bind("xsd", XSD)
    g.bind("schema", SDO)
    g.bind("exa", EXA)
    g.bind("ext", EXT)
    g.bind("dbo", DBO)

    for film in json_data:
        film_uri = EXA[film["Title"].replace(" ", "_")]
        g.add((film_uri, RDF.type, DBO.Film))
        g.add((film_uri, RDFS.label, Literal(film["Title"], lang="en")))

        director_uri = EXA[film["Director"].replace(" ", "_")]
        g.add((film_uri, DBO.director, director_uri))
        g.add((director_uri, RDF.type, EXT.Director))

        for actor in film["Starring"].split(", "):
            actor_uri = EXA[actor.replace(" ", "_")]
            g.add((film_uri, DBO.starring, actor_uri))
            g.add((actor_uri, RDF.type, EXT.Actor))

        # numerics are not treated as nodes
        g.add((film_uri, DBO.releaseDate, Literal(film["ReleaseDate"], datatype=XSD.date)))

        g.add((film_uri, DBO.runtime, Literal(film["Runtime"], datatype= XSD.integer)))

        for genre in film["Genre"].split(", "):
            genre_uri = SDO[genre.replace(" ", "_")]
            g.add((film_uri, DBO.genre, genre_uri))

    return g

if __name__ == "__main__":
    data_input = input("Enter the file name of the input data: ")

    with open(data_input, "r") as file:
        json_data = json.load(file)

    rdf_graph = create_rdf_from_json(json_data)

    print(rdf_graph.serialize(format="turtle"))

    # convert the graph() to turtle for dbpedia
    rdf_graph.serialize(destination="movies.ttl", format="turtle")