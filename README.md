python code required for Deterministic Atlas Analysis  

Running Deformetrica  
https://doi.org/10.1007/978-3-030-04747-4_1  
https://doi.org/10.1016/j.neuroimage.2014.06.043  
  
And processing outputs, using a modified version of the code from Toussaint et al 2021 https://doi.org/10.1242/dev.188631  

batch_ply_to_VTK_conversion.py  
For batch converting a folder of .ply meshes into .vtk format for analysis in python  
  
dataxml_Generation.py  
For generating the data_set.xml file needed for Deformetrica  
  
Processing_Deformetrica_outputs.py  
For installing and running Deformetrica  
Then processing the outputs using kernel principal components analysis, following Toussaint et al 2021 https://doi.org/10.1242/dev.188631  
  
create_datacsv_fromxml.py  
For generating data.csv file needed for postprocessing the outputs of Deformetrica  
