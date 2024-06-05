# running pyconcepticon on terminal (on Git Bash Windows)
## assuming I created a new virtual environment ".venv" in `cldf` directory using the following commands
cd "C:\Users\GRajeg\OneDrive - Nexus365\Documents\cldf"
python -m venv .venv

## then already installed the relevant Python packages
pip install pyconcepticon

## and already downloaded the Concepticon database in the `concepticon-data/concepticondata` directory

# go the the relevant directory
cd "C:\Users\GRajeg\OneDrive - Nexus365\Documents\cldf"

# activate the virtual environment
source .venv/Scripts/activate

# go the concepticon directory
cd concepticon-data

# run the concept mapping
concepticon map_concepts C:/Users/GRajeg/OneDrive\ -\ Nexus365/Documents/Research/enolex-2023-05-23/data/enolex-gloss-to-map_2024-1810.tsv --output C:/Users/GRajeg/OneDrive\ -\ Nexus365/Documents/Research/enolex-2023-05-23/data/enolex-gloss-mapped-to-edit_2024-1810.tsv

# go back to EnoLEX directory
cd "C:/Users/GRajeg/OneDrive - Nexus365/Documents/Research/enolex-2023-05-23"
