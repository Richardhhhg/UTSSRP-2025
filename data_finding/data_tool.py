import pandas as pd
import numpy as np
from habanero import Crossref
from habanero import counts
from habanero import cn
import re

chem = pd.read_csv('Chemistry publication record.csv')
phys = pd.read_csv('Physics publication record.csv', encoding='latin-1')
med = pd.read_csv('Medicine publication record.csv', encoding='latin-1')

cr = Crossref()

female_laureates = ["mayer, mg", "cori, gt", "yalow, rs", "mcclintock, b",
                    "levimontalcini, r", "elion, gb", "nussleinvolhard, c",
                    "buck, l", "barresinoussi, f", "blackburn, eh",
                    "greider, cw", "moser, ei", "youyou tu", "curie, i",
                    "hodgkin, d", "yonath, a"]

def find_p(laureate, field):
    if field == "chem":
        df = chem
    elif field == "phys":
        df = phys
    else:
        df = med
    data = df[df["Laureate name"] == laureate]
    sorted_data = data.sort_values(by='Pub year')
    sorted_data.reset_index(drop=True, inplace=True)
    return float((sorted_data[sorted_data['Is prize-winning paper'] == 'YES'].index[0] + 1)
                 / sorted_data.shape[0])
def find_all_chem_p():
    chem_laureates = {}
    chem_unique_names = chem['Laureate name'].unique()
    for name in chem_unique_names:
        chem_laureates.update({name: find_p(name, "chem")})
    return chem_laureates

def find_phys_p():
    phys_laureates = {}
    phys_unique_names = phys['Laureate name'].unique()
    for name in phys_unique_names:
        phys_laureates.update({name: find_p(name, "phys")})
    return phys_laureates

def find_med_p():
    med_laureates = {}
    med_unique_names = med['Laureate name'].unique()
    for name in med_unique_names:
        med_laureates.update({name: find_p(name, "chem")})
    return med_laureates

def find_team_size(doi):
    doi_content = cn.content_negotiation(ids = doi)
    match = re.search(r'author\s*=\s*{([^}]*)}', doi_content)
    if match:
        authors_str = match.group(1)
        num_authors = len([a.strip() for a in authors_str.split(' and ')])
    else:
        print("Author field not found.")
        num_authors = 0
    return num_authors

def find_all_team_sizes(laureate, field):
    team_sizes = {}
    if field == "chem":
        df = chem
    elif field == "phys":
        df = phys
    else:
        df = med
    laureate_data = df[df['Laureate name'] == laureate]
    for row in laureate_data.itertuples():
        team_sizes.update({row.DOI: find_team_size(row.DOI)})
    return team_sizes

def find_citations(doi):
    return counts.citation_count(doi= doi)

def find_all_citation_counts(laureate, field):
    """habanero takes forever, so use this function at your own risk"""
    paper_citations = {}
    if field == "chem":
        df = chem
    elif field == "phys":
        df = phys
    else:
        df = med
    laureate_data = df[df['Laureate name'] == laureate]
    for row in laureate_data.itertuples():
        paper_citations.update({row.DOI: counts.citation_count(doi= row.DOI)})
    return paper_citations

def determine_gender(laureate):
    if laureate in female_laureates:
        return "female"
    else:
        return "male"

def determine_laureate_genders(field):
    laureate_genders = {}
    if field == "chem":
        df = chem
    elif field == "phys":
        df = phys
    else:
        df = med
    laureate_names = df['Laureate name'].unique()
    for name in laureate_names:
        laureate_genders.update({name: determine_gender(name)})
    return laureate_genders


