A collection of helpful troubleshooting notes or links collected as the app was being expanded.  

## TROUBLESHOOTING

1. **Clear environment**: In the console, type `rm(list=ls())`  
2. **Restart R**: From RStudio, select Session --> Restart R  
3. **Rerun code**  

***

### Specific errors  

**Error message**: *Density plot error: could not find function "expansion"*  
**Solution**: Update ggplot2  

**Error message**: *Error in .getReactiveEnvironment()$currentContext() : Operation not allowed without an active reactive context. (You tried to do something that can only be done from inside a reactive expression or observer.)*  
**Solution**: This is a *shiny* error that means, for example, that you tried to do something with a reactive variable outside of a reactive expression like reactive, observe, eventReactive, observeEvent, or a render function. Make sure all your reactive variables are within these types of expressions.  

***


## HELPFUL NOTES AND LINKS

NASA binned data and projections for MODIS-Aqua  
https://oceancolor.gsfc.nasa.gov/docs/format/l3bins/  
https://spatialreference.org/ref/sr-org/28/  

Turn a dataframe of irregularly-spaced points into a raster (to plot on a leaflet map)  
http://chris35wills.github.io/gridding_data/  

Overlaying a table containing expressions on a ggplot  
https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r  
https://stackoverflow.com/questions/43714570/add-superscript-in-table-using-tablegrob  

Get max bin height from a ggplot histogram (geom_histogram)  
https://community.rstudio.com/t/geom-histogram-max-bin-height/10026/2  

Write output to a txt file  
https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r  

Document outline tips  
https://support.rstudio.com/hc/en-us/articles/200484568-Code-Folding-and-Sections  

CSS for "accordion" button style (in sidebar)  
https://www.w3schools.com/howto/howto_js_accordion.asp  

nls (non-linear least squares regression) issues:  
https://stackoverflow.com/questions/33265467/nls-troubles-missing-value-or-an-infinity-produced-when-evaluating-the-model  


***

## SHINY

Shiny control widgets  
https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/  

Formatting spacing/divisions of a column in the UI:  

* br(),       add a blank line  
* hr(),       add a horizontal bar  

Styling hr() with css:  
https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar?noredirect=1&lq=1  

Layout help:  
https://stackoverflow.com/questions/53519783/set-minimum-maximum-width-for-box-with-shinydashboard-r  
https://stackoverflow.com/questions/44486551/how-to-set-the-sizes-and-specific-layouts-in-shiny  
https://getbootstrap.com/docs/4.0/layout/grid/  
https://shiny.rstudio.com/articles/layout-guide.html  

Listen for more than one event to trigger a section of code in a shiny app  
https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha  

Differences between reactive functions in shiny  
https://riptutorial.com/shiny/topic/10787/reactive--reactivevalue-and-eventreactive--observe-and-observeevent-in-shiny  

| FUNCTION | TRIGGER | EFFECT |
|----------|---------|--------|
| observe | any of its reactive variables change somewhere else | some side effect, doesn't return anything |
| observeEvent | events specified in first argument | some side effect, doesn't return anything |
| reactive | any of its reactive variables change somewhere else | returns a value in the reactive or render object where it's called, changing it |
| eventReactive | events specified in first argument | returns a value in the reactive or render object where it's called, changing it |

`reactiveValues()`:  
"input" is a list of reactive values, but creating another reactiveValues list (the "state" list in PhytoFit) gives you more control over what user input
will trigger which functions, and whether or not the values in stats/plots/etc use the current user (widget) input.  
EXAMPLE specific to PhytoFit:  

    # The user might select a different year but NOT click the "load" button,
    # then select a different day, which triggers the map to update. How would
    # the date stamp on the map behave:
    # If it used input$year: the date displayed would be the new year, but the data
    #                        on the map would be for the old year because the new
    #                        annual data hasn't been loaded yet
    # If it used state$year: the date stamp would match the data currently loaded,
    #                        because state$year isn't changed until "load" is clicked,
    #                        so it's updated at the same time as the annual data
    #                        (input$year and state$year are different here)

To wrap the text in an actionButton:  
`actionButton(inputId, label, width, style = "white-space: normal;")`  

Hide download buttons if data has not been loaded yet:  
https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler  

Make shiny wait between character strokes in a numericInput or textInput bar using "debounce" on a reactive function.  
https://stackoverflow.com/questions/34937698/can-i-let-shiny-wait-for-a-longer-time-for-numericinput-before-updating  
https://community.rstudio.com/t/stop-shiny-animation-instantly/8709  
"One idea is to use the debounce() function to make Shiny skip over frames if it can't keep up."  

Side-by-side widgets:  
https://stackoverflow.com/questions/36709441/how-to-display-widgets-inline-in-shiny  

sliderInput number formatting (for years):  
https://stackoverflow.com/questions/26636335/formatting-number-output-of-sliderinput-in-shiny  

Progress bars:  
https://cran.r-project.org/web/packages/shinybusy/vignettes/shinybusy-usage.html  

Show/hide entire panels based on condition:  
https://stackoverflow.com/questions/45180395/can-you-use-shinyjs-to-hide-show-whole-panels  


***

## LEAFLET

Add multiple polygons to a map with the sp package, lapply, and one instance of addPolygons when creating the base leaflet map  
https://stackoverflow.com/questions/37578260/how-can-i-use-a-for-loop-to-map-multiple-polygons-with-the-leaflet-within-shin  

Draw custom polygons  
https://stackoverflow.com/questions/48858581/get-coordinates-from-a-drawing-object-from-an-r-leaflet-map  
https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R  
https://stackoverflow.com/questions/44979900/how-to-download-polygons-drawn-in-leaflet-draw-as-geojson-file-from-r-shiny  

Leaflet projections  
https://rstudio.github.io/leaflet/projections.html  

Use leaflet and leafletProxy to create a background map and update it later without recreating the whole map  
https://rstudio.github.io/leaflet/shiny.html  

Instead of using raster, use circle markers and plot the binned data straight to the map. Much slower, but here are tips to speed it up:  
https://community.rstudio.com/t/plotting-thousands-of-points-in-leaflet-a-way-to-improve-the-speed/8196  

Add text (title) to Leaflet map and change its position  
https://stackoverflow.com/questions/49072510/r-add-title-to-leaflet-map  
https://stackoverflow.com/questions/28680940/text-is-breaking-using-absolute-positioning  
https://stackoverflow.com/questions/18719376/shift-block-with-position-absolute/18719512  

Saving a leaflet map that uses leafletProxy() to update is too advanced to figure out right now (mapshot, leaflet-easyprint not working), so instead I'm
using downloadButton in the UI, corresponding downloadHandler in the server, and saveWidget within downloadHandler (or ggsave or write.csv for the plots
and csv files) to allow the user to save the map widget in its current state just by clicking a download button.  
https://stackoverflow.com/questions/52210682/download-leaflet-map-from-a-shiny-app-hosted-on-shiny-io  

Control layer order of leaflet map with addMapPane  
https://stackoverflow.com/questions/33143169/how-to-get-layer-to-top-in-shiny-leaflet-map  
BUT...  
If you use addMapPane, the map is saved with "SVG" instead of on a canvas, i.e. the "preferCanvas" option does nothing  
(preferCanvas significantly speeds it up when plotting thousands of points, so this is more important than addMapPane)  
https://github.com/Leaflet/Leaflet/issues/5190  

Get coordinates when user clicks on point on the map:  
https://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle  

Format popups and create tables  
https://stackoverflow.com/questions/49033505/leaflet-r-addpopups-coordinates-properties  
https://groups.google.com/forum/#!topic/shiny-discuss/SesZv0j-cNE  

Programmatically delete drawn polygons (instead of making the user delete them with the draw toolbar):  
https://github.com/bhaskarvk/leaflet.extras/issues/96  

*March 2020*: Tried using patchwork for the output plots, but the bloomfit click didn't work after this. Forgot to copy links but very briefly skimmed one where a user said it would not work. Possibly something to do with how patchwork prints plots (or doesn't print them...?) but I don't understand how that works, so went back to the old method for now. (The purpose of this patchwork detour was to find a better way to display statistics/parameter tables next to the plots)  

Using leafletProxy, can't use removeDrawToolbar... except with difficulty, maybe:  
https://stackoverflow.com/questions/52622993/leaflet-drawtoolbar-with-leafletproxy-in-r-shiny  
https://github.com/r-spatial/mapedit/issues/61  
https://github.com/bhaskarvk/leaflet.extras/pull/184  
[As of Jan 2021, the leaflet.extras fix works on the github version, but not DMapps]  
