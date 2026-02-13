# Deformetrica  -NOT IN PYTHON

# Activate environment
source activate deformetrica

# Navigate to the file directory
cd
ls
cd .. # for moving up a directory

# Deformetrica installation
conda create -n deformetrica python=3.7
source activate deformetrica
conda install -c pytorch -c conda-forge -c anaconda -c aramislab/label/rc deformetrica

# run Deformetrica in command line on linux or mac: NOT IN PYTHON
############################################################################
deformetrica estimate model.xml data_set.xml --p optimization_parameters.xml -v INFO
############################################################################

# Everything below is post-processing from Deformetrica results 
# IN PYTHON ################################################################
# This is modified from the Landmark-free Morphometry pipeline from Toussaint et al 2021

python

### General Imports
import matplotlib.pyplot as plt
from glob import glob as glob
import numpy as np
import pandas as pd
import os
import matplotlib.cm as cm

### VTK imports
import vtk
from vtk import vtkPolyDataReader
from vtk import vtkPolyDataWriter
from vtk import vtkUnstructuredGridReader
from vtk import vtkUnstructuredGridWriter
from vtk import vtkPolyData
from vtk import vtkPoints
from vtk import vtkCellArray
from vtk import vtkProcrustesAlignmentFilter as Procrustes
from vtk import vtkMultiBlockDataGroupFilter as GroupFilter
from vtk import vtkLandmarkTransform
from vtk import vtkTransformPolyDataFilter as TransformFilter
from vtk.util import vtkConstants
from vtk import vtkIdList
from vtk import vtkIdTypeArray
from vtk import vtkTriangle
from vtk import vtkFloatArray
from vtk import vtkTetra
from vtk import vtkMath


# Load Data

Working_dir = os.getcwd()
os.chdir(os.path.join(Working_dir, 'output'))
os.getcwd()
controlpoints = np.loadtxt('DeterministicAtlas__EstimatedParameters__ControlPoints.txt')
f = open('DeterministicAtlas__EstimatedParameters__Momenta.txt')
first_line = f.readline().split(' ')
number_of_subjects = int(first_line[0])
number_of_controlpoints = int(first_line[1])
dimension = int(first_line[2])
f.close()
momenta = np.loadtxt('DeterministicAtlas__EstimatedParameters__Momenta.txt', skiprows=2)
momenta_linearised = momenta.reshape([number_of_subjects, dimension*number_of_controlpoints])

print('Control Points: {}'.format(number_of_controlpoints))
print('Subjects: {}'.format(number_of_subjects))
print('Dimension: {}'.format(dimension))

# Define supgroups
df = pd.read_csv ('data.csv') # make this beforehand using 'create_datacsv_fromxml.py'
df



##################### KPCA #######################################################################

from matplotlib import pyplot as plt
from sklearn.decomposition import PCA, KernelPCA
from sklearn.svm import SVC
from sklearn.model_selection import permutation_test_score
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_score
fig = plt.figure(figsize=(7,5))
idx = [0,1]


kpca = KernelPCA(kernel="rbf", fit_inverse_transform=True, n_components=(number_of_subjects-1), gamma= .00000025)
# Change n_components to the number of components you need
X_kpca = kpca.fit_transform(momenta_linearised)

# Saving data
# Create a DataFrame with all PCs at once
pcs_df = pd.DataFrame(X_kpca, columns=[f'PC{i+1}' for i in range(X_kpca.shape[1])])

# Concatenate with the original DataFrame
df = pd.concat([df.reset_index(drop=True), pcs_df], axis=1)
    
################################################### Run above separately
df.to_csv('kpca.csv')
df                                                                 

# Eigenvalues

eigenvalues = kpca.eigenvalues_
eigenvectors = kpca.eigenvectors_ / np.linalg.norm(kpca.eigenvectors_, axis=0)

eig = pd.DataFrame()
eig['PCA dimension'] = ['PC{}'.format(idx+1) for idx in range(len(eigenvalues))]
eig['cum. variability (in %)'] = 100 * np.cumsum(eigenvalues) / np.sum(eigenvalues)
pd.set_option('precision', 2)
eig['lambda'] = eigenvalues
eig['alpha'] = list(np.transpose(eigenvectors))

eig
eig.to_csv('eig.csv')


#



