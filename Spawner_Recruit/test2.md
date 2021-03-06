

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

You can use $$\LaTeX$$ to typeset formulas. A formula can be displayed inline, e.g. $$e=mc^2$$, or as a block:
$$\int_\Omega \nabla u \cdot \nabla v~dx = \int_\Omega fv~dx$$
Also check out this [LaTeX introduction](https://en.wikibooks.org/wiki/LaTeX/Mathematics).



```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

Text formatting 
------------------------------------------------------------

*italic*  or _italic_
**bold**   __bold__
`code`
superscript^2^ and subscript~2~

Headings
------------------------------------------------------------

# 1st Level Header

## 2nd Level Header

### 3rd Level Header

Lists
------------------------------------------------------------

*   Bulleted list item 1

*   Item 2

    * Item 2a

    * Item 2b

1.  Numbered list item 1

1.  Item 2. The numbers are incremented automatically in the output.

Links and images
------------------------------------------------------------

<http://example.com>

[linked phrase](http://example.com)

![optional caption text](Data_input_run.PNG){width=10%}

Tables 
------------------------------------------------------------
<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class="ox-hugo-table basic-styling">
<div></div>
<div class="table-caption">
<span class="table-number"></span>

</div>
S-R input table example 

| Year   | Spawner| Recruit| 
|-------:|-------:|-------:|
| 1966   | 1000   | 2500   |
| 1967   | 1200   | 7300   |
| 1968   | 2500   | 4250   |
| 1969   | 3500   | 5250   |
</div>



#Test 

First Header  | Second Header
------------- | -------------
Content Cell  | Content Cell
Content Cell  | Content Cell


ANOVA


|   Source    |  df  |   Sum of Sq   |    Mean Sq    |    F   |  $Pr(>F_{1,29})$    |
|:------------|-----:|--------------:|--------------:|-------:|--------------------:|
|  Girth      |  *1* |  7581.8       |  7581.8       | 419.26 |  **< 2.2e-16**      |
|  Residual   |  29  |  524.3        |   18.1        |        |                     |




Run input table example 

| Year   | Spawner| Run    | Age 3  |  Age 4 | Age 5  | Age 6  | 
|-------:|-------:|-------:|-------:|-------:|-------:|-------:|
| 1966   | 1000   | 4500   | 0.01   | 0.45   | 0.43   |  0.06  |
| 1967   | 1200   | 6000   | 0.03   | 0.48   | 0.42   |  0.07  |
| 1968   | 2500   | 8250   | 0.01   | 0.42   | 0.52   |  0.06  |
| 1969   | 3500   | 12500  | 0.05   | 0.56   | 0.38   |  0.01  |


Escapement Only input table example 

| Year   | Spawner| 
|-------:|-------:|
| 1966   | 1000   |
| 1967   | 1200   |
| 1968   | 2500   |
| 1969   | 3500   | 





