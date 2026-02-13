import os
import vtk

# specify the directory containing the .ply files
input_dir = r"D:/fibula"
# now run!

###################################################################
ply_files = [f for f in os.listdir(input_dir) if f.endswith('.ply')]

output_dir = os.path.join(input_dir, "vtk")
if not os.path.exists(output_dir):
    os.mkdir(output_dir)

for ply_file in ply_files:
    # create a reader for the ply file
    reader = vtk.vtkPLYReader()
    reader.SetFileName(os.path.join(input_dir, ply_file))
    reader.Update()
    # get the output of the reader (vtkPolyData object)
    polydata = reader.GetOutput()
    # create a writer for the vtk file
    vtk_file = os.path.splitext(ply_file)[0] + '.vtk'  # replace .ply extension with .vtk
    vtk_path = os.path.join(output_dir, vtk_file)
    writer = vtk.vtkPolyDataWriter()
    writer.SetFileName(vtk_path)
    writer.SetInputData(polydata)
    writer.Write()
    print(f"Converted {ply_file} to {vtk_file}")
