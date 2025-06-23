# Interactive Inventory Map - R, Excel API, Leaflet JS

This repository contains a pilot test of how public data usually stored in excel spreadsheets can be quickly be transformed into sharable update APIs easily integrated into various decentalised platforms. The repository is made up of the following files:
* [quick\_clean\_dlg\_data.R](./quick_clean_dlg_data.R): Contains reusable functions to do fast importation of multiple data files, cleaning, merging and exporting into an comma-delimited csv file. **NB**: The current functions only deal with .csv format files. Ensure files are in csv format.
* [map\_render.html](./map_render.html): An html file used to render an interactive map in a browser. The map imports data from an API interface containing all map water infrastructure data points from three districts (`Yumbe`, `Madi Okollo` & `Terego) all in West Nile Region of Uganda. [west-nile-water-supply-inventory API GET Endpoint](https://excelapi-studio.vercel.app/api/projects/west-nile-water-supply-inventory/endpoints/west-nile-water-supply-inventory)

## Built With :email:
- [JavaScript](https://javascript.info/) - Supports interactive map components in the `map\_render.html` file
- [Leaflet](https://leafletjs.com) - Javascript based library to support building of interactive maps in the browser
- [Cascading Style Sheets](https://developer.mozilla.org) - Styling language to support basic styling of the interactive map
- [R Project](https://www.r-project.org) - Programming Language for Statistical computing and graphics
- [ExcelAPI Studio](https://excelapi-studio.vercel.app) - New web application that supports instant REST API generation from excel based spreadsheets

## Authors :black\_nib:
- Raymond Lukwago - [Github](https://github.com/lukwagoraymond) / [LinkedIn](https://www.linkedin.com/in/raymondlukwago/)

## Acknowledgements :pray:

All work contained in this weekend project was completed by me that simply started as an excitment to try out a new web application **ExcelAPI Studio** but quickly evolved into a 5-hour project aimed at testing a theoratical idea on hoe best to *technically* able three districts have online access to their water supply and sanitation related data. Kudos to the chaps at ExcelAPI studio for building such a good platform. I hope it gets its traction within the development cooperation and financial space someday. 
