## Embedded Apps for StatPREP

StatPREP tutorials can be written with embedded apps to perform computation. These can be effective for interactive demonstration or for instances where the instructor wants to allow the student to see the result of a calculation without any R code showing.

### Taking a look

To take a quick look at an app in this package, use the `select_app()` function.

```r
library(LittleApps)
select_app()
```

This will let you choose an app and will run it in your system viewer.

### Embedding an app in a document

When you want to include an app in a tutorial or other interactive document (with `runtime: shiny`), refer to the app in a chunk in your document. The chunk will look like this:

````
```{r child = system.file("density-graphs.Rmd", package = "LittleApps")}
```
````

