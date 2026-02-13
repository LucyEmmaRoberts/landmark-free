import xml.etree.ElementTree as ET
import csv

# change path to your data_set.xml
xml_file_path = r'/media/goswamilab/GoodBoy/Lucy_DAA/2026/ulna_wild_Jan2026/ulna_wild/data_set.xml'

# change path to your Deformetrica output folder
csv_file_path = r'/media/goswamilab/GoodBoy/Lucy_DAA/2026/ulna_wild_Jan2026/ulna_wild/output/data.csv' # change output path

# now run!
######################################################################
tree = ET.parse(xml_file_path)
root = tree.getroot()

filenames = []
for subject in root.findall('subject'):
    filename_elem = subject.find('./visit/filename')
    if filename_elem is not None:
        filenames.append(filename_elem.text)

with open(csv_file_path, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['id'])  # Header
    for name in filenames:
        writer.writerow([name])

print(f"{len(filenames)} filenames written to {csv_file_path}")

