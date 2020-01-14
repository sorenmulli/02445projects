import json
import numpy as np
import os, sys

os.chdir(sys.path[0])

with open('armdata.json') as f:
	data = json.load(f)

data = np.array(data)
data = data[3]

print(data.shape)