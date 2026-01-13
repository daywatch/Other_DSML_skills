from SPARQLWrapper import SPARQLWrapper, JSON

sparql_wrapper = SPARQLWrapper("http://dbpedia.org/sparql")
sparql_wrapper.setReturnFormat(JSON)

def run_sparql_query(sparql_param, sparql_file):
    with open(sparql_file, "r") as file:
        query = file.read().replace("{sparql_param}", sparql_param)
    sparql_wrapper.setQuery(query)

    try:
        results = sparql_wrapper.queryAndConvert()
        print(results)
        return results
    except Exception as e:
        print(e)

if __name__ == "__main__":
    film_name = input("Enter the film name: ")
    run_sparql_query(film_name, "./sparql_queries/film_construct.sparql")