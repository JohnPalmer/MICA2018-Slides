# importa paquets
import pysal as ps
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.formula.api import ols
import os
import math

# Comprova el directori de treball
os.getcwd()

# Canvia el directori de treball
os.chdir('/Users/palmer/Dropbox/upf_criminology/2017/slides/MICA2017/data')
# especifica la ruta de l'arxiu de formes
shp_path = 'nyc_assaults_subway_data.shp'

# càrrega l'arxiu de formes
D = ps.pdio.read_files(shp_path)

qW = ps.queen_from_shapefile(shp_path)

mi = ps.Moran(D.n_assaults[:, None], qW, two_tailed=False)

mi.I
mi.EI
