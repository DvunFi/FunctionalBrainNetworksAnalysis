import os
from typing import Dict

import igraph
import pandas as pd


def get_files_names(path) -> Dict[str, list]:
    input_dirs = os.scandir(path)
    result = {}
    for input_dir in input_dirs:
        with os.scandir(path + "/" + input_dir.name) as input_files:
            list_of_csv_files = []
            for input_file in input_files:
                list_of_csv_files.append(input_file)
        result[input_dir.name] = list_of_csv_files
    return result


def calculate_basics_graph_measures(csv_files_dictionary, output_dir_path):
    col_names = ['V', 'E', 'mean_degree', 'max_connected',"assortativity_degree", "csv_file_name", "subject_name"]
    for key in csv_files_dictionary:
        df = pd.DataFrame(columns=col_names)
        for csv_file in csv_files_dictionary[key]:
            adjacency_matrix = list()
            for line in open(csv_file, 'r').readlines():
                adjacency_matrix.append([int(value) for value in line.split(',')])

            g = igraph.Graph.Adjacency(adjacency_matrix, igraph.ADJ_UNDIRECTED)
            df_row = {'V': g.vcount()
                      , 'E': g.ecount()
                      , 'mean_degree': igraph.mean(g.degree())
                      , "max_connected": g.clusters().giant().vcount()
                      , "assortativity_degree": igraph.Graph.assortativity_degree(g)
                      , "csv_file_name": csv_file.name
                      , "subject_name": key
                      }
            df.loc[len(df)] = df_row
            print(csv_file.name+" measures were calculated")
            print("\n")

        excel_dir_path = output_dir_path + "/" + key + "/tables/"
        file_name = key + "_basics_measures.xlsx"
        if not os.path.exists(excel_dir_path):
            os.makedirs(excel_dir_path)
        writer = pd.ExcelWriter(excel_dir_path + file_name, engine='xlsxwriter')
        df.to_excel(writer, sheet_name='Sheet1')
        workbook = writer.book
        workbook.close()
