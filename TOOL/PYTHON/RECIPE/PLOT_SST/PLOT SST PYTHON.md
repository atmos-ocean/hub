# PLOT SST PYTHON

https://python.plainenglish.io/how-to-retrieve-and-visualize-sea-surface-temperature-data-using-python-60ce6fc199e6

```python
#import requests
import xarray as xr
import matplotlib.pyplot as plt
import cmocean
import cartopy.crs as ccrs
import cartopy.feature as cfeature
#from datetime import datetime, timedelta

INFLE=""
dataset = xr.open_dataset(INFLE)

# Extract relevant data
sst = dataset['analysed_sst'][0].values
lats = dataset['latitude'].values
lons = dataset['longitude'].values

# Create a Cartopy plot
plt.figure(figsize=(10, 8))
ax = plt.axes(projection=ccrs.PlateCarree())
ax.add_feature(cfeature.COASTLINE)
ax.add_feature(cfeature.BORDERS, linestyle=':')
ax.gridlines(draw_labels=True)

# Plot sea surface temperature
cax = plt.pcolormesh(lons, lats, sst, shading='auto', cmap=cmocean.cm.thermal, transform=ccrs.PlateCarree())
plt.colorbar(cax, label='Sea Surface Temperature (C)', orientation='horizontal', pad=0.06)
        
plt.title('Sea Surface Temperature')
FIG="sst_plot.png"
plt.savefig(FIG, dpi=300, bbox_inches="tight")
print("INFLE: "+INFLE)
print("FIG: "+FIG)

```

