# Landmark-free morphometry  

Code from The impact of alignment and scaling on biological inferences from landmark-free morphometrics (Roberts, Camaiti & Goswami, In review @ _Journal of Anatomy_)

For aligning meshes, running Deformetrica, and processing outputs.

## Aligning meshes in R  

Mesh_alignment.R
* For aligning a set of meshes in R
* Using the PAMS protocol following Roberts _et al._ 2026

Alignment_code_SMA_PAIS_PAMS.R
* For running different alignment protocols, according to Roberts _et al._ 2026

## Generating files for Deformetrica
          
batch_ply_to_VTK_conversion.py  
* For batch converting a folder of .ply meshes into .vtk format for analysis in python  
  
dataxml_Generation.py  
* For generating the data_set.xml file needed for Deformetrica  
  
create_datacsv_fromxml.py  
* For generating data.csv file needed for postprocessing the outputs of Deformetrica  

## Running deformetrica  

install_run_deformetrica.txt
* Instructions for installing and running deformetrica in command line

## Postprocessing Deformetrica outputs

Processing_Deformetrica_outputs.py
* Modified from the landmark-free morphometry pipeline from Toussaint _et al._ 2021
* kernel principal components analysis
* Extraction of kPCs, eigenvalues and eigenvectors

## References  
https://doi.org/10.1007/978-3-030-04747-4_1  
https://doi.org/10.1016/j.neuroimage.2014.06.043  
https://doi.org/10.1242/dev.188631  
