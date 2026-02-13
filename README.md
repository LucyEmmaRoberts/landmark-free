# Landmark-free morphometry  

For aligning meshes, running Deformetrica, and processing outputs

## Aligning meshes in R  

Mesh_alignment.R
* For aligning a set of meshes in R
* Using the PAMS protocol following Roberts _et al._ 2026

Alignment_code_SMA_PAIS_PAMS.R
* For running different alignment protocols, according to Roberts _et al._ 2026

## Useful bits of code for generating files for Deformetrica
          
batch_ply_to_VTK_conversion.py  
* For batch converting a folder of .ply meshes into .vtk format for analysis in python  
  
dataxml_Generation.py  
* For generating the data_set.xml file needed for Deformetrica  
  
create_datacsv_fromxml.py  
* For generating data.csv file needed for postprocessing the outputs of Deformetrica  

## References  
https://doi.org/10.1007/978-3-030-04747-4_1  
https://doi.org/10.1016/j.neuroimage.2014.06.043  
https://doi.org/10.1242/dev.188631  
