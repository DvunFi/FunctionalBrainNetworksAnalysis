import datetime
from NetworkAnalise.networks.general_functions import *

print("igraph version: " + igraph.__version__)

pathToInputDirs = "../../../input/adjacencyMatrices"
pathToOutputDir = "../../../output"
csv_files_list = get_files_names(pathToInputDirs)

time1 = datetime.datetime.now()

calculate_basics_graph_measures(csv_files_list, pathToOutputDir)

time2 = datetime.datetime.now()

print(time1)
print(time2)
