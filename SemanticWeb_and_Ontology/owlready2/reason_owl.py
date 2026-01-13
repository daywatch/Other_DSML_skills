from owlready2 import *

tbox = get_ontology("file://film_tbox.owl").load()
alignment = get_ontology("file://film_alignment.owl").load()

abox = get_ontology("file://film_abox.owl").load()

abox.imported_ontologies.append(tbox)
abox.imported_ontologies.append(alignment)

with abox:
    sync_reasoner_pellet(infer_property_values = True, infer_data_property_values = True)

abox.save(file="inferred_abox.owl", format="rdfxml")

print("Reasoning process is finished")

film_parasite = abox.Parasite

print(film_parasite.label)
print(film_parasite.runtime)
print(film_parasite.hasActor)
print(film_parasite.hasActor[0].label)
print(film_parasite.hasActor[0].hasCollaboratedWith)