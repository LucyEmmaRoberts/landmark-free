import os
from xml.etree.ElementTree import Element, SubElement, tostring

# Define the directory containing the files
directory = r"D:/fibula"
os.chdir(directory)
# now run!

################################################################

file_list = []

for filename in os.listdir(directory):
    if filename.endswith('.vtk'):
        file_list.append(filename)

sorted_files = sorted(file_list)

xml_data = '<?xml version="1.0"?>\n<data-set>\n'

for filename in sorted_files:
    xml_data += '    <subject id="' + filename + '">\n'
    xml_data += '        <visit id="mesh">\n'
    xml_data += '            <filename object_id="mesh">' + filename + '</filename>\n'
    xml_data += '        </visit>\n'
    xml_data += '    </subject>\n'

xml_data += '</data-set>\n'

with open('data_set.xml', 'w') as f:
    f.write(xml_data)
