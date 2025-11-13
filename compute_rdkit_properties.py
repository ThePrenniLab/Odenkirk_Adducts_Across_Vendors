import pandas as pd
from rdkit import Chem
from rdkit.Chem import Descriptors, rdmolops

# Load CSV containing SMILES
df = pd.read_csv(r"R:/Group Members/Melanie Odenkirk/adducts_across_vendors_2025/irts_list_for_analysis.csv")

# Prepare descriptor list
Exact_Mass = []
Formal_Charge = []
LogP = []
TPSA = []
HBA = []
HBD = []
RotB = []
Refractivity = []
Polarizability_Est = []

# Placeholders for external descriptors
Physiological_Charge = []
logS = []
pKa_Strongest_Acidic = []
pKa_Strongest_Basic = []

for smi in df["SMILES"]:
    mol = Chem.MolFromSmiles(smi)
    if mol is not None:
        Exact_Mass.append(Descriptors.ExactMolWt(mol))
        Formal_Charge.append(rdmolops.GetFormalCharge(mol))  # updated
        LogP.append(Descriptors.MolLogP(mol))
        TPSA.append(Descriptors.TPSA(mol))
        HBA.append(Descriptors.NumHAcceptors(mol))
        HBD.append(Descriptors.NumHDonors(mol))
        RotB.append(Descriptors.NumRotatableBonds(mol))
        Refractivity.append(Descriptors.MolMR(mol))
        Polarizability_Est.append(Descriptors.MolLogP(mol))  # approximate
    else:
        Exact_Mass.append(None)
        Formal_Charge.append(None)
        LogP.append(None)
        TPSA.append(None)
        HBA.append(None)
        HBD.append(None)
        RotB.append(None)
        Refractivity.append(None)
        Polarizability_Est.append(None)
    
    # placeholders
    Physiological_Charge.append(None)
    logS.append(None)
    pKa_Strongest_Acidic.append(None)
    pKa_Strongest_Basic.append(None)

# Add descriptors to dataframe
df["Exact_Mass"] = Exact_Mass
df["Formal_Charge"] = Formal_Charge
df["Physiological_Charge"] = Physiological_Charge
df["LogP_ALOGPS"] = LogP
df["logS"] = logS
df["pKa_Strongest_Acidic"] = pKa_Strongest_Acidic
df["pKa_Strongest_Basic"] = pKa_Strongest_Basic
df["Hydrogen_Acceptor_Count"] = HBA
df["Hydrogen_Donor_Count"] = HBD
df["Polar_Surface_Area"] = TPSA
df["Rotatable_Bond_Count"] = RotB
df["Refractivity"] = Refractivity
df["Polarizability"] = Polarizability_Est

# Save to CSV
df.to_csv("irts_full_rdkit_properties.csv", index=False)
print("Done! Saved descriptors to irts_full_rdkit_properties.csv")