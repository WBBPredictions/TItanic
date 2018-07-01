import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from random import choices

def randomReplace():
	#importing train data into a pandas dataframe
	df = pd.read_csv('train.csv', index_col=False)
	#finding the frequencies of people's ages
	freqs = df['Age'].value_counts(normalize = True, ascending = True)
	#storing those frequencies and using them to define a probability distribution
	dic = freqs.to_dict()
	ages = list(dic.keys())
	values = list(dic.values())
	#randomly choosing ages to fill the NaN values
	rand_ages = choices(ages, values, k = 177)
	#replacing the NaN values with the random ages
	df.loc[df.Age.isnull(), 'Age'] = rand_ages
	print(df.to_string())

def automaticInterpolate():
	