import json
import numpy as np
import os, sys

def dataload(path = 'armdata.json'):
	with open('armdata.json') as f:
		data = json.load(f)

	data = np.array(data)
	#Only experiment 4
	data = data[3]
	data = data.astype(np.float64)

	#Flatten data into 100 observations with 300 attributes. The first 10 observations are the first person. The first 100 attributes are the x coordinates.
	data = data.reshape(100, 100, 3)
	data = data.reshape(100, 300, order = 'F')

	targets = np.concatenate([[i]*10 for i in range(0,10)])
	return data, targets




if __name__ == "__main__":
	os.chdir(sys.path[0])
	data, targets = dataload()

