# Species distribution model tool
## For local ubuntu server
server connection
$ssh mman@192.168.3.237

Call from dir: /mman/home/czechgrids_local/R_outputs 

R -q --vanilla < /home/mman/czechgrids_local/R_outputs/manual_predictions_server.R > manual_predictions_server.output

R -q --vanilla < manual_predictions_server.R > manual_predictions_server.output

outputs are stored in /mman/home/czechgrids_local/R_outputs 

Warning: 
R output "manual_predictions_server.output" is overwritten in any run

### Script run for whole cr or subset
Default is for whole CR. In case of subset, needs to be manually edited around lines 99-100 and 276-286. Or search for "!!edit"

### Params
In header of "manual_predictions_server.R" there is list of task params. 

* taxon_name="Lewinskya rupestris" #select the name of species from DaLiBor database
* includes_absence=FALSE # default. DaLiBor data never includes absence
* pseudoabsence_count=161 # default. generate the same number of psudo absence as presence data count
* epsg=4326 # default, DaLiBor data are in WGS84
* area_type="cr" # variable, select are type, default=cr, in case of other type, scrips needs to be manually edited
* area_buffer=3000 # buffer size in case of buffer area_type
