import pandas as pd
accidents = pd.read_csv("Accidents(2014-2017)_Mapped.csv", low_memory=False)
casualties = pd.read_csv("Casualties(2014-2017)_Mapped.csv", low_memory=False)
vehicles = pd.read_csv("Vehicles(2014-2017)_Mapped.csv", low_memory=False)

accsheff = accidents[accidents["Local_Authority_(District)"]== "Sheffield"]
accsheff.info()
mcas = pd.merge(accsheff, casualties, how='left', on='Accident_Index')
mcas.info()
mveh = pd.merge(accsheff, vehicles, how='left', on='Accident_Index')
mveh.info()
