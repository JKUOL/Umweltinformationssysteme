---
title: "2. Fish Classifier"
author: "Gruppe 23 Justin König, Ali Abdullah, Rita Tagoula Ngoufo"
date: "10.05.2023"
output:
  html_document:
    keep_md: yes
    code_folding: hide
    theme: yeti
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
editor_options:
  markdown:
    wrap: 72
header-includes:
   - \usepackage{ragged2e}
---



<style type="text/css">
body {
  text-align: justify;
}
</style>
<style type="text/css">
.gt_table {
  margin-left: auto;
  margin-right: auto;
}
</style>
# Daten

Ein Datensatz, der aus Länge1, Länge2, Länge3, Höhe, Breite, Geschlecht und 
Gewicht von 7 verschiedenen Fischarten besteht, wurde für die Analyse verwendet. 
Ziel war es, einen Klassifikator zu entwickeln, der die Fischspezies basierend 
auf den Merkmalen Länge1, Länge2, Länge3, Höhe und Breite in einem Testdatensatz 
vorhersagen kann. Der Testdatensatz wurde aus jeder 10. Zeile des ursprünglichen 
Datensatzes gebildet. Die verbleibenden Daten wurden als Trainingsdatensatz
verwendet, um den Klassifikator zu trainieren und dessen Leistung anhand des
Testdatensatzes zu bewerten.


```r
# load data frame
fish_df <- read.csv2("fishcatch.csv")

# remove unnecessary data
fish_df <- fish_df[48:nrow(fish_df),]

colnames(fish_df) <- c("Species", "Length1", "Length2", "Length3","Height", 
                       "Width", "sex", "Weight")

fish_df <- as.data.frame(apply(fish_df, 2, function(x) gsub(",", ".", x)))

# List the columns you want to convert to numeric
columns_to_convert <- c("Length1", "Length2", "Length3","Height", "Width",
                       "Weight")

# Convert the specified columns to numeric
fish_df <- fish_df %>%
  mutate(across(all_of(columns_to_convert), as.numeric))

# training data (data frame without every 10th row)
train_data <- fish_df[!(seq_len(nrow(fish_df)) %% 10 == 0), ]

# test df (every 10. Fish)
test_data <- fish_df[seq(1, nrow(fish_df), by = 10), ]

# counts fish classes
fish_count <- train_data %>%
  group_by(Species) %>%
  summarise(Count = n())

# creates a tibble out of the df
fish_count <- as_tibble(fish_count)
```

Im folgenden Diagramm ist die Anzahl der jeweiligen Fischspezies im 
Trainingsatensatz dargstellt.


```r
# creates the theme tt2 with minimal settings
tt2 <- ttheme_minimal()

# creates a grob to put inside of the bar plot
fish_count_grob <- tableGrob(fish_count, theme = tt2, rows = NULL)

fish_count_plot <- ggplot(fish_count, aes(x = Species, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_classic() +
  labs(title = "Fish Count per Species",
       x = "Species",
       y = "Count")

fish_count_plot_with_table <- fish_count_plot +
  annotation_custom(
    grob = fish_count_grob, 
    xmin = 5, xmax = 5.5, ymin = 20, ymax = 50
  )
print(fish_count_plot_with_table)
```

<img src="Fish-Classifier_files/figure-html/visualitation-1.png" style="display: block; margin: auto;" />

Die Normalverteilungsparameter sind in der folgender Tabelle zusammengefasst.


```r
# group by species
grouped_fish <- fish_df %>%
  group_by(Species)

# calculates mean and standarddeviation
normal_params <- grouped_fish %>%
  summarise(
    "mean L1" = mean(Length1, na.rm = TRUE),
    "mean L2" = mean(Length2, na.rm = TRUE),
    "mean L3" = mean(Length3, na.rm = TRUE),
    "mean H" = mean(Height, na.rm = TRUE),
    "mean W" = mean(Width, na.rm = TRUE),
    "SD L1" = sd(Length1, na.rm = TRUE),
    "SD L2" = sd(Length2, na.rm = TRUE),
    "SD L3" = sd(Length3, na.rm = TRUE),
    "SD H" = sd(Height, na.rm = TRUE),
    "SD W" = sd(Width, na.rm = TRUE),
  )

rounded_normal_params <- normal_params %>%
  mutate_if(is.numeric, round, digits = 2)

# creates a table out of the tibble
normal_params_gt <- gt(rounded_normal_params)

# changes to the table
normal_params_gt <- 
  normal_params_gt %>%
  # Titel
  tab_header(
    title = "Normal distribution parameters ",
  )

normal_params_gt
```

```{=html}
<div id="zceuqpvkby" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zceuqpvkby table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#zceuqpvkby thead, #zceuqpvkby tbody, #zceuqpvkby tfoot, #zceuqpvkby tr, #zceuqpvkby td, #zceuqpvkby th {
  border-style: none;
}

#zceuqpvkby p {
  margin: 0;
  padding: 0;
}

#zceuqpvkby .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zceuqpvkby .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#zceuqpvkby .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zceuqpvkby .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zceuqpvkby .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zceuqpvkby .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zceuqpvkby .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zceuqpvkby .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zceuqpvkby .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zceuqpvkby .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zceuqpvkby .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zceuqpvkby .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zceuqpvkby .gt_spanner_row {
  border-bottom-style: hidden;
}

#zceuqpvkby .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#zceuqpvkby .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zceuqpvkby .gt_from_md > :first-child {
  margin-top: 0;
}

#zceuqpvkby .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zceuqpvkby .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zceuqpvkby .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#zceuqpvkby .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#zceuqpvkby .gt_row_group_first td {
  border-top-width: 2px;
}

#zceuqpvkby .gt_row_group_first th {
  border-top-width: 2px;
}

#zceuqpvkby .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zceuqpvkby .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#zceuqpvkby .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#zceuqpvkby .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zceuqpvkby .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zceuqpvkby .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zceuqpvkby .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#zceuqpvkby .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zceuqpvkby .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zceuqpvkby .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zceuqpvkby .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zceuqpvkby .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zceuqpvkby .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#zceuqpvkby .gt_left {
  text-align: left;
}

#zceuqpvkby .gt_center {
  text-align: center;
}

#zceuqpvkby .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zceuqpvkby .gt_font_normal {
  font-weight: normal;
}

#zceuqpvkby .gt_font_bold {
  font-weight: bold;
}

#zceuqpvkby .gt_font_italic {
  font-style: italic;
}

#zceuqpvkby .gt_super {
  font-size: 65%;
}

#zceuqpvkby .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#zceuqpvkby .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#zceuqpvkby .gt_indent_1 {
  text-indent: 5px;
}

#zceuqpvkby .gt_indent_2 {
  text-indent: 10px;
}

#zceuqpvkby .gt_indent_3 {
  text-indent: 15px;
}

#zceuqpvkby .gt_indent_4 {
  text-indent: 20px;
}

#zceuqpvkby .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="11" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Normal distribution parameters </td>
    </tr>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Species">Species</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean L1">mean L1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean L2">mean L2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean L3">mean L3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean H">mean H</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean W">mean W</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD L1">SD L1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD L2">SD L2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD L3">SD L3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD H">SD H</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="SD W">SD W</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Species" class="gt_row gt_right">1</td>
<td headers="mean L1" class="gt_row gt_right">30.33</td>
<td headers="mean L2" class="gt_row gt_right">33.14</td>
<td headers="mean L3" class="gt_row gt_right">38.39</td>
<td headers="mean H" class="gt_row gt_right">39.59</td>
<td headers="mean W" class="gt_row gt_right">14.15</td>
<td headers="SD L1" class="gt_row gt_right">3.64</td>
<td headers="SD L2" class="gt_row gt_right">3.97</td>
<td headers="SD L3" class="gt_row gt_right">4.22</td>
<td headers="SD H" class="gt_row gt_right">1.60</td>
<td headers="SD W" class="gt_row gt_right">0.78</td></tr>
    <tr><td headers="Species" class="gt_row gt_right">2</td>
<td headers="mean L1" class="gt_row gt_right">28.80</td>
<td headers="mean L2" class="gt_row gt_right">31.32</td>
<td headers="mean L3" class="gt_row gt_right">34.32</td>
<td headers="mean H" class="gt_row gt_right">29.20</td>
<td headers="mean W" class="gt_row gt_right">15.90</td>
<td headers="SD L1" class="gt_row gt_right">5.58</td>
<td headers="SD L2" class="gt_row gt_right">5.72</td>
<td headers="SD L3" class="gt_row gt_right">6.02</td>
<td headers="SD H" class="gt_row gt_right">1.35</td>
<td headers="SD W" class="gt_row gt_right">1.82</td></tr>
    <tr><td headers="Species" class="gt_row gt_right">3</td>
<td headers="mean L1" class="gt_row gt_right">20.64</td>
<td headers="mean L2" class="gt_row gt_right">22.27</td>
<td headers="mean L3" class="gt_row gt_right">24.97</td>
<td headers="mean H" class="gt_row gt_right">26.74</td>
<td headers="mean W" class="gt_row gt_right">14.61</td>
<td headers="SD L1" class="gt_row gt_right">3.46</td>
<td headers="SD L2" class="gt_row gt_right">3.65</td>
<td headers="SD L3" class="gt_row gt_right">4.03</td>
<td headers="SD H" class="gt_row gt_right">1.49</td>
<td headers="SD W" class="gt_row gt_right">0.78</td></tr>
    <tr><td headers="Species" class="gt_row gt_right">4</td>
<td headers="mean L1" class="gt_row gt_right">18.73</td>
<td headers="mean L2" class="gt_row gt_right">20.35</td>
<td headers="mean L3" class="gt_row gt_right">22.79</td>
<td headers="mean H" class="gt_row gt_right">39.31</td>
<td headers="mean W" class="gt_row gt_right">14.08</td>
<td headers="SD L1" class="gt_row gt_right">3.28</td>
<td headers="SD L2" class="gt_row gt_right">3.56</td>
<td headers="SD L3" class="gt_row gt_right">3.96</td>
<td headers="SD H" class="gt_row gt_right">1.43</td>
<td headers="SD W" class="gt_row gt_right">0.60</td></tr>
    <tr><td headers="Species" class="gt_row gt_right">5</td>
<td headers="mean L1" class="gt_row gt_right">11.26</td>
<td headers="mean L2" class="gt_row gt_right">11.92</td>
<td headers="mean L3" class="gt_row gt_right">13.04</td>
<td headers="mean H" class="gt_row gt_right">16.89</td>
<td headers="mean W" class="gt_row gt_right">10.22</td>
<td headers="SD L1" class="gt_row gt_right">1.22</td>
<td headers="SD L2" class="gt_row gt_right">1.43</td>
<td headers="SD L3" class="gt_row gt_right">1.43</td>
<td headers="SD H" class="gt_row gt_right">1.13</td>
<td headers="SD W" class="gt_row gt_right">1.29</td></tr>
    <tr><td headers="Species" class="gt_row gt_right">6</td>
<td headers="mean L1" class="gt_row gt_right">42.48</td>
<td headers="mean L2" class="gt_row gt_right">45.48</td>
<td headers="mean L3" class="gt_row gt_right">48.72</td>
<td headers="mean H" class="gt_row gt_right">15.84</td>
<td headers="mean W" class="gt_row gt_right">10.44</td>
<td headers="SD L1" class="gt_row gt_right">9.03</td>
<td headers="SD L2" class="gt_row gt_right">9.71</td>
<td headers="SD L3" class="gt_row gt_right">10.17</td>
<td headers="SD H" class="gt_row gt_right">1.00</td>
<td headers="SD W" class="gt_row gt_right">0.75</td></tr>
    <tr><td headers="Species" class="gt_row gt_right">7</td>
<td headers="mean L1" class="gt_row gt_right">25.74</td>
<td headers="mean L2" class="gt_row gt_right">27.89</td>
<td headers="mean L3" class="gt_row gt_right">29.57</td>
<td headers="mean H" class="gt_row gt_right">26.26</td>
<td headers="mean W" class="gt_row gt_right">15.84</td>
<td headers="SD L1" class="gt_row gt_right">8.56</td>
<td headers="SD L2" class="gt_row gt_right">9.02</td>
<td headers="SD L3" class="gt_row gt_right">9.53</td>
<td headers="SD H" class="gt_row gt_right">1.91</td>
<td headers="SD W" class="gt_row gt_right">1.36</td></tr>
  </tbody>
  
  
</table>
</div>
```

# Visualisierung

Im folgenden sind Diagramme dargestellt, welche die Abhängigkeiten der einzelnen 
Variablen untereinander darstellen.

```r
# Create a long-format data frame
long_fish_1_df <- fish_df %>%
  dplyr::select(Species, Length1, Length2, Length3, Height, Width) %>%
  tidyr::gather(key = "Feature", value = "Value", -c(Species, Length1))

long_fish_2_df <- fish_df %>%
  dplyr::select(Species, Length1, Length2, Length3, Height, Width) %>%
  tidyr::gather(key = "Feature", value = "Value", -c(Species, Length2))

long_fish_3_df <- fish_df %>%
  dplyr::select(Species, Length1, Length2, Length3, Height, Width) %>%
  tidyr::gather(key = "Feature", value = "Value", -c(Species, Length3))

long_fish_4_df <- fish_df %>%
  dplyr::select(Species, Length1, Length2, Length3, Height, Width) %>%
  tidyr::gather(key = "Feature", value = "Value", -c(Species, Height))

long_fish_5_df <- fish_df %>%
  dplyr::select(Species, Length1, Length2, Length3, Height, Width) %>%
  tidyr::gather(key = "Feature", value = "Value", -c(Species, Width))

ggplot(long_fish_1_df, aes(x = Length1, y = Value, color = factor(Species))) +
  geom_point() +
  labs(x = "Length1", y = "Value", color = "Species") +
  theme_minimal() +
  facet_wrap(~Feature, ncol = 4) +
  ggtitle("Scatter plots of Length1 vs. other features")
```

<img src="Fish-Classifier_files/figure-html/plotting-1.png" style="display: block; margin: auto;" />

```r
ggplot(long_fish_2_df, aes(x = Length2, y = Value, color = factor(Species))) +
  geom_point() +
  labs(x = "Length2", y = "Value", color = "Species") +
  theme_minimal() +
  facet_wrap(~Feature, ncol = 4) +
  ggtitle("Scatter plots of Length2 vs. other features")
```

<img src="Fish-Classifier_files/figure-html/plotting-2.png" style="display: block; margin: auto;" />

```r
ggplot(long_fish_3_df, aes(x = Length3, y = Value, color = factor(Species))) +
  geom_point() +
  labs(x = "Length3", y = "Value", color = "Species") +
  theme_minimal() +
  facet_wrap(~Feature, ncol = 4) +
  ggtitle("Scatter plots of Length3 vs. other features")
```

<img src="Fish-Classifier_files/figure-html/plotting-3.png" style="display: block; margin: auto;" />

```r
ggplot(long_fish_4_df, aes(x = Height, y = Value, color = factor(Species))) +
  geom_point() +
  labs(x = "Height", y = "Value", color = "Species") +
  theme_minimal() +
  facet_wrap(~Feature, ncol = 4) +
  ggtitle("Scatter plots of Height vs. other features")
```

<img src="Fish-Classifier_files/figure-html/plotting-4.png" style="display: block; margin: auto;" />

```r
ggplot(long_fish_5_df, aes(x = Width, y = Value, color = factor(Species))) +
  geom_point() +
  labs(x = "Width", y = "Value", color = "Species") +
  theme_minimal() +
  facet_wrap(~Feature, ncol = 4) +
  ggtitle("Scatter plots of Width vs. other features")
```

<img src="Fish-Classifier_files/figure-html/plotting-5.png" style="display: block; margin: auto;" />



# Klassifikator

Zunächst werden die einzigartigen Fischarten aus den Trainingsdaten ermittelt.
Für jede Fischart werden daraufhin die Mittelwerte, Kovarianzmatrizen und 
a-priori Wahrscheinlichkeiten berechnet.

Anschließend wird die Dichte der multivariaten Gaußverteilung für jeden 
gegebenen Datenpunkt und die zugehörigen Verteilungsparameter ermittelt.

Die LDA- und QDA-Classifier nutzen die Maximum-Likelihood-Methode, um Fische 
basierend auf den Merkmalen zu klassifizieren. Hierbei wird für jeden Datenpunkt 
die bedingte Wahrscheinlichkeit jeder Fischart berechnet und mit der 
entsprechenden a-priori Wahrscheinlichkeit multipliziert. Die Fischart mit der 
höchsten resultierenden Wahrscheinlichkeit wird als vorhergesagte Spezies 
ausgewählt. Der Unterschied zwischen der LDA- und QDA-Methode liegt in der 
Annahme bezüglich der Kovarianzmatrizen: Bei der LDA wird eine gemeinsame 
Kovarianzmatrix für alle Klassen angenommen, während bei der QDA jeder Klasse 
eine individuelle Kovarianzmatrix zugeordnet wird. Dies führt dazu, dass 
QDA-Classifier flexibler sind und komplexere Trennlinien zwischen den Klassen 
ermöglichen, während LDA-Classifier weniger anfällig für Überanpassung sind, 
sofern die Annahme einer gemeinsamen Kovarianzmatrix zutreffend ist.

Abschließend wird der Klassifikator auf den Testdatensatz angewendet, und die 
Genauigkeit ermittelt, indem die Anzahl der korrekt vorhergesagten Spezies 
durch die Gesamtanzahl der Vorhersagen geteilt wird.


```r
# Calculate the mean, covariance, and a-priori probability for each fish species
species_list <- unique(train_data$Species)
stats_list <- lapply(species_list, function(species) {
  species_data <- train_data[train_data$Species == 
                               species, c("Length1", "Length2", 
                                          "Length3", "Height", "Width")]
  n <- nrow(species_data)
  
  list(mean = colMeans(species_data),
       cov = cov(species_data) + diag(1e-6, ncol(species_data)),
       prior = n / nrow(train_data))
})

names(stats_list) <- species_list

# Multivariate Gaussian density function
multi_gau <- function(x, mean, cov) {
  k <- length(mean)
  exp(-0.5 * t(x - mean) %*% solve(cov, x - mean)) / sqrt((2 * pi)^k * det(cov))
}

### QDA ###

# classifier
classifier_fish_qda <- function(lengths) {
  likelihoods <- sapply(stats_list, function(params) {
    likelihood <- multi_gau(lengths, params$mean, params$cov)
    likelihood * params$prior
  })
  
  names(likelihoods)[which.max(likelihoods)]
}

# Classify each fish in the test set
predicted_species_qda <- apply(test_data[, c("Length1", "Length2", "Length3", 
                                             "Height", "Width")], 
                               1, classifier_fish_qda)

### LDA ###

# Calculate the common covariance matrix for LDA
common_cov <- cov(train_data[, c("Length1", "Length2", "Length3", "Height", 
                                 "Width")]) + 
  diag(1e-6, ncol(train_data[, c("Length1", 
                                 "Length2", "Length3", "Height", "Width")]))

# LDA classifier
classifier_fish_lda <- function(lengths) {
  likelihoods <- sapply(stats_list, function(params) {
    likelihood <- multi_gau(lengths, params$mean, common_cov)
    likelihood * params$prior
  })
  
  names(likelihoods)[which.max(likelihoods)]
}

# Classify each fish in the test set
predicted_species_lda <- apply(test_data[, c("Length1", 
                                             "Length2", "Length3",
                                             "Height", "Width")], 
                               1, classifier_fish_lda)


# summary of species in test data and predicted species
comparision_qda_df <- data.frame(Species = test_data$Species, 
                                 Prediction = predicted_species_qda)
comparision_lda_df <- data.frame(Species = test_data$Species,
                                 Prediction = predicted_species_lda)

# Calculate accuracy
correct_predictions_lda <- sum(predicted_species_lda == test_data$Species)
correct_predictions_qda<- sum(predicted_species_qda == test_data$Species)

# total predictions
total_predictions <- length(predicted_species_lda)


accuracy_lda <- correct_predictions_lda / total_predictions
accuracy_qda <- correct_predictions_qda / total_predictions
```

In der folgenden Tabelle, ist die Maximum-Liklyhood zusammengefasst. Grüne 
Zellen wurde korrekt vorhergesagt, rosane falsch.

```r
# LDA classifier with likelihood percentages
classifier_fish_lda_percent <- function(lengths) {
  likelihoods <- sapply(stats_list, function(params) {
    likelihood <- multi_gau(lengths, params$mean, common_cov)
    likelihood * params$prior
  })

  likelihood_percentages <- likelihoods / sum(likelihoods) * 100
  return(likelihood_percentages)
}

# QDA classifier with likelihood percentages
classifier_fish_qda_percent <- function(lengths) {
  likelihoods <- sapply(stats_list, function(params) {
    likelihood <- multi_gau(lengths, params$mean, params$cov)
    likelihood * params$prior
  })

  likelihood_percentages <- likelihoods / sum(likelihoods) * 100
  return(likelihood_percentages)
}

# Apply the modified classifiers to the test data
lda_likelihood_percentages <- t(apply(test_data[, c("Length1",
                                                    "Length2", "Length3", 
                                                    "Height", "Width")], 
                                      1, classifier_fish_lda_percent))
qda_likelihood_percentages <- t(apply(test_data[, c("Length1", 
                                                    "Length2", "Length3", 
                                                    "Height", "Width")], 
                                      1, classifier_fish_qda_percent))

# Create data frames for the likelihood percentages
lda_percent_df <- data.frame(lda_likelihood_percentages, 
                             Species = test_data$Species)
qda_percent_df <- data.frame(qda_likelihood_percentages, 
                             Species = test_data$Species)

# Number of decimal places you want to round to
decimal_places <- 1000

# Round all numbers in the data frame, excluding the Species column
lda_percent_df_rounded <- 
  as.data.frame(lapply(lda_percent_df[, -ncol(lda_percent_df)], 
                       function(x) round(x, decimal_places)))
lda_percent_df_rounded$Species <- lda_percent_df$Species 

# Round all numbers in the data frame, excluding the Species column
qda_percent_df_rounded <- 
  as.data.frame(lapply(qda_percent_df[, -ncol(qda_percent_df)], 
                       function(x) round(x, decimal_places)))
qda_percent_df_rounded$Species <- qda_percent_df$Species  

# Format all numbers in the data frame, excluding the Species column
lda_percent_df_sci <- 
  as.data.frame(lapply(lda_percent_df_rounded[, -ncol(lda_percent_df_rounded)],
                       function(x) formatC(x, format = "e", digits = 2)))
lda_percent_df_sci$Species <- lda_percent_df_rounded$Species  

# Format all numbers in the data frame, excluding the Species column
qda_percent_df_sci <- 
  as.data.frame(lapply(qda_percent_df_rounded[, -ncol(qda_percent_df_rounded)], 
                       function(x) formatC(x, format = "e", digits = 2)))
qda_percent_df_sci$Species <- qda_percent_df_rounded$Species  

lda_percent_df_sci$Prediction <- predicted_species_lda
qda_percent_df_sci$Prediction <- predicted_species_qda

# find the column with the max value for each row
lda_percent_df_sci$max_col <- apply(lda_percent_df_sci, 1, 
                                    function(x) which.max(as.numeric(x)))

# create gt table
lda_percent_gt_sci <- gt(lda_percent_df_sci)

# iterate over rows of the table to color the max value cell
for (i in seq_len(nrow(lda_percent_df_sci))) {
  lda_percent_gt_sci <- tab_style(
    lda_percent_gt_sci,
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = lda_percent_df_sci$max_col[i],
      rows = i
    )
  )
}

# changes to table
lda_percent_gt_sci <- 
  lda_percent_gt_sci %>%
  cols_hide(columns = "max_col") %>% 
  # Title
  tab_header(
    title = "Maximum-Liklyhood-Selections LDA-Classifier",
    subtitle = "in Percent [%]"
  ) %>%
  # color cell
  tab_style(
    style = cell_fill(color = "lightpink"),
    locations = cells_body(
      columns = c(7),
      rows = 6)
    ) %>% 
  tab_style(
    style = cell_fill(color = "lightgoldenrodyellow"),
    locations = cells_body(
      columns = c(3),
      rows = 6)
    )

qda_percent_df <- qda_percent_df_sci

# find the column with the max value for each row
qda_percent_df$max_col <- apply(qda_percent_df, 1, 
                                function(x) which.max(as.numeric(x)))

# create gt table
qda_percent_gt_sci <- gt(qda_percent_df)

# iterate over rows of the table to color the max value cell
for (i in seq_len(nrow(qda_percent_df))) {
  qda_percent_gt_sci <- tab_style(
    qda_percent_gt_sci,
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = qda_percent_df$max_col[i],
      rows = i
    )
  )
}
# changes to table
qda_percent_gt_sci <- 
  qda_percent_gt_sci %>%
  cols_hide(columns = "max_col") %>% 
  # Title
  tab_header(
    title = "Maximum-Liklyhood-Selections QDA-Classifier",
    subtitle = "in Percent [%]"
  )

lda_percent_gt_sci
```

```{=html}
<div id="rlevxkaclc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rlevxkaclc table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rlevxkaclc thead, #rlevxkaclc tbody, #rlevxkaclc tfoot, #rlevxkaclc tr, #rlevxkaclc td, #rlevxkaclc th {
  border-style: none;
}

#rlevxkaclc p {
  margin: 0;
  padding: 0;
}

#rlevxkaclc .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rlevxkaclc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rlevxkaclc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rlevxkaclc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rlevxkaclc .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rlevxkaclc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rlevxkaclc .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rlevxkaclc .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rlevxkaclc .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rlevxkaclc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rlevxkaclc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rlevxkaclc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rlevxkaclc .gt_spanner_row {
  border-bottom-style: hidden;
}

#rlevxkaclc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rlevxkaclc .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rlevxkaclc .gt_from_md > :first-child {
  margin-top: 0;
}

#rlevxkaclc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rlevxkaclc .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rlevxkaclc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rlevxkaclc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rlevxkaclc .gt_row_group_first td {
  border-top-width: 2px;
}

#rlevxkaclc .gt_row_group_first th {
  border-top-width: 2px;
}

#rlevxkaclc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlevxkaclc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rlevxkaclc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rlevxkaclc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rlevxkaclc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlevxkaclc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rlevxkaclc .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rlevxkaclc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rlevxkaclc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rlevxkaclc .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rlevxkaclc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlevxkaclc .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rlevxkaclc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rlevxkaclc .gt_left {
  text-align: left;
}

#rlevxkaclc .gt_center {
  text-align: center;
}

#rlevxkaclc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rlevxkaclc .gt_font_normal {
  font-weight: normal;
}

#rlevxkaclc .gt_font_bold {
  font-weight: bold;
}

#rlevxkaclc .gt_font_italic {
  font-style: italic;
}

#rlevxkaclc .gt_super {
  font-size: 65%;
}

#rlevxkaclc .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rlevxkaclc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rlevxkaclc .gt_indent_1 {
  text-indent: 5px;
}

#rlevxkaclc .gt_indent_2 {
  text-indent: 10px;
}

#rlevxkaclc .gt_indent_3 {
  text-indent: 15px;
}

#rlevxkaclc .gt_indent_4 {
  text-indent: 20px;
}

#rlevxkaclc .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_title gt_font_normal" style>Maximum-Liklyhood-Selections LDA-Classifier</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>in Percent [%]</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X1">X1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X2">X2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X3">X3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X4">X4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X5">X5</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X6">X6</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X7">X7</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Species">Species</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Prediction">Prediction</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">8.36e+01</td>
<td headers="X2" class="gt_row gt_left">1.41e+00</td>
<td headers="X3" class="gt_row gt_left">8.10e+00</td>
<td headers="X4" class="gt_row gt_left">7.42e-01</td>
<td headers="X5" class="gt_row gt_left">8.09e-01</td>
<td headers="X6" class="gt_row gt_left">1.44e-01</td>
<td headers="X7" class="gt_row gt_left">5.15e+00</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">8.74e+01</td>
<td headers="X2" class="gt_row gt_left">1.54e+00</td>
<td headers="X3" class="gt_row gt_left">6.86e+00</td>
<td headers="X4" class="gt_row gt_left">1.68e-01</td>
<td headers="X5" class="gt_row gt_left">1.86e-01</td>
<td headers="X6" class="gt_row gt_left">1.47e-01</td>
<td headers="X7" class="gt_row gt_left">3.70e+00</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">8.77e+01</td>
<td headers="X2" class="gt_row gt_left">6.49e-01</td>
<td headers="X3" class="gt_row gt_left">4.73e+00</td>
<td headers="X4" class="gt_row gt_left">2.17e+00</td>
<td headers="X5" class="gt_row gt_left">2.13e-01</td>
<td headers="X6" class="gt_row gt_left">2.75e-01</td>
<td headers="X7" class="gt_row gt_left">4.30e+00</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">9.24e+01</td>
<td headers="X2" class="gt_row gt_left">2.19e+00</td>
<td headers="X3" class="gt_row gt_left">5.05e-01</td>
<td headers="X4" class="gt_row gt_left">1.02e-01</td>
<td headers="X5" class="gt_row gt_left">1.63e-02</td>
<td headers="X6" class="gt_row gt_left">3.20e-01</td>
<td headers="X7" class="gt_row gt_left">4.48e+00</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">6.15e+00</td>
<td headers="X2" class="gt_row gt_left">4.35e+00</td>
<td headers="X3" class="gt_row gt_left" style="background-color: #90EE90;">5.58e+01</td>
<td headers="X4" class="gt_row gt_left">5.86e-02</td>
<td headers="X5" class="gt_row gt_left">1.06e+01</td>
<td headers="X6" class="gt_row gt_left">1.10e-01</td>
<td headers="X7" class="gt_row gt_left">2.29e+01</td>
<td headers="Species" class="gt_row gt_right">3</td>
<td headers="Prediction" class="gt_row gt_right">3</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.34e+01</td>
<td headers="X2" class="gt_row gt_left">1.34e+01</td>
<td headers="X3" class="gt_row gt_left" style="background-color: #FAFAD2;">3.14e+01</td>
<td headers="X4" class="gt_row gt_left">1.94e-02</td>
<td headers="X5" class="gt_row gt_left">1.40e+00</td>
<td headers="X6" class="gt_row gt_left">2.35e-01</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #FFB6C1;">4.02e+01</td>
<td headers="Species" class="gt_row gt_right">3</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.06e-01</td>
<td headers="X2" class="gt_row gt_left">2.72e-03</td>
<td headers="X3" class="gt_row gt_left">1.35e-02</td>
<td headers="X4" class="gt_row gt_left" style="background-color: #90EE90;">9.93e+01</td>
<td headers="X5" class="gt_row gt_left">2.58e-02</td>
<td headers="X6" class="gt_row gt_left">1.87e-04</td>
<td headers="X7" class="gt_row gt_left">5.12e-01</td>
<td headers="Species" class="gt_row gt_right">4</td>
<td headers="Prediction" class="gt_row gt_right">4</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">9.98e+00</td>
<td headers="X2" class="gt_row gt_left">3.95e-01</td>
<td headers="X3" class="gt_row gt_left">1.31e+00</td>
<td headers="X4" class="gt_row gt_left" style="background-color: #90EE90;">6.90e+01</td>
<td headers="X5" class="gt_row gt_left">2.88e-01</td>
<td headers="X6" class="gt_row gt_left">5.58e-02</td>
<td headers="X7" class="gt_row gt_left">1.90e+01</td>
<td headers="Species" class="gt_row gt_right">4</td>
<td headers="Prediction" class="gt_row gt_right">4</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">5.76e-01</td>
<td headers="X2" class="gt_row gt_left">2.87e-01</td>
<td headers="X3" class="gt_row gt_left">1.27e+01</td>
<td headers="X4" class="gt_row gt_left">2.94e-02</td>
<td headers="X5" class="gt_row gt_left" style="background-color: #90EE90;">8.18e+01</td>
<td headers="X6" class="gt_row gt_left">6.69e-01</td>
<td headers="X7" class="gt_row gt_left">3.93e+00</td>
<td headers="Species" class="gt_row gt_right">5</td>
<td headers="Prediction" class="gt_row gt_right">5</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.01e+00</td>
<td headers="X2" class="gt_row gt_left">2.22e-01</td>
<td headers="X3" class="gt_row gt_left">5.93e-01</td>
<td headers="X4" class="gt_row gt_left">4.49e-02</td>
<td headers="X5" class="gt_row gt_left">2.89e+00</td>
<td headers="X6" class="gt_row gt_left" style="background-color: #90EE90;">8.91e+01</td>
<td headers="X7" class="gt_row gt_left">6.14e+00</td>
<td headers="Species" class="gt_row gt_right">6</td>
<td headers="Prediction" class="gt_row gt_right">6</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">6.07e-02</td>
<td headers="X2" class="gt_row gt_left">9.20e-03</td>
<td headers="X3" class="gt_row gt_left">3.84e-03</td>
<td headers="X4" class="gt_row gt_left">1.69e-04</td>
<td headers="X5" class="gt_row gt_left">4.58e-03</td>
<td headers="X6" class="gt_row gt_left" style="background-color: #90EE90;">9.97e+01</td>
<td headers="X7" class="gt_row gt_left">1.75e-01</td>
<td headers="Species" class="gt_row gt_right">6</td>
<td headers="Prediction" class="gt_row gt_right">6</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">2.13e+00</td>
<td headers="X2" class="gt_row gt_left">5.67e+00</td>
<td headers="X3" class="gt_row gt_left">3.67e+00</td>
<td headers="X4" class="gt_row gt_left">2.25e-01</td>
<td headers="X5" class="gt_row gt_left">1.63e+00</td>
<td headers="X6" class="gt_row gt_left">2.53e-01</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">8.64e+01</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.07e+00</td>
<td headers="X2" class="gt_row gt_left">1.60e+01</td>
<td headers="X3" class="gt_row gt_left">1.19e+01</td>
<td headers="X4" class="gt_row gt_left">2.14e-03</td>
<td headers="X5" class="gt_row gt_left">2.35e-01</td>
<td headers="X6" class="gt_row gt_left">5.57e-02</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">7.07e+01</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.21e+00</td>
<td headers="X2" class="gt_row gt_left">5.64e+00</td>
<td headers="X3" class="gt_row gt_left">3.71e+00</td>
<td headers="X4" class="gt_row gt_left">6.49e-02</td>
<td headers="X5" class="gt_row gt_left">3.77e-01</td>
<td headers="X6" class="gt_row gt_left">2.53e-01</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">8.87e+01</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">9.62e-01</td>
<td headers="X2" class="gt_row gt_left">3.63e+00</td>
<td headers="X3" class="gt_row gt_left">3.66e+00</td>
<td headers="X4" class="gt_row gt_left">9.59e-02</td>
<td headers="X5" class="gt_row gt_left">3.78e-02</td>
<td headers="X6" class="gt_row gt_left">1.46e-01</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">9.15e+01</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.68e+00</td>
<td headers="X2" class="gt_row gt_left">5.28e+00</td>
<td headers="X3" class="gt_row gt_left">1.95e+00</td>
<td headers="X4" class="gt_row gt_left">4.20e-02</td>
<td headers="X5" class="gt_row gt_left">2.17e-02</td>
<td headers="X6" class="gt_row gt_left">6.20e-01</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">9.04e+01</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
  </tbody>
  
  
</table>
</div>
```

```r
qda_percent_gt_sci
```

```{=html}
<div id="jafwafxohf" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jafwafxohf table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jafwafxohf thead, #jafwafxohf tbody, #jafwafxohf tfoot, #jafwafxohf tr, #jafwafxohf td, #jafwafxohf th {
  border-style: none;
}

#jafwafxohf p {
  margin: 0;
  padding: 0;
}

#jafwafxohf .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jafwafxohf .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jafwafxohf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jafwafxohf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jafwafxohf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jafwafxohf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jafwafxohf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jafwafxohf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jafwafxohf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jafwafxohf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jafwafxohf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jafwafxohf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jafwafxohf .gt_spanner_row {
  border-bottom-style: hidden;
}

#jafwafxohf .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#jafwafxohf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jafwafxohf .gt_from_md > :first-child {
  margin-top: 0;
}

#jafwafxohf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jafwafxohf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jafwafxohf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#jafwafxohf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#jafwafxohf .gt_row_group_first td {
  border-top-width: 2px;
}

#jafwafxohf .gt_row_group_first th {
  border-top-width: 2px;
}

#jafwafxohf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jafwafxohf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jafwafxohf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jafwafxohf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jafwafxohf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jafwafxohf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jafwafxohf .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jafwafxohf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jafwafxohf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jafwafxohf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jafwafxohf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jafwafxohf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jafwafxohf .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jafwafxohf .gt_left {
  text-align: left;
}

#jafwafxohf .gt_center {
  text-align: center;
}

#jafwafxohf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jafwafxohf .gt_font_normal {
  font-weight: normal;
}

#jafwafxohf .gt_font_bold {
  font-weight: bold;
}

#jafwafxohf .gt_font_italic {
  font-style: italic;
}

#jafwafxohf .gt_super {
  font-size: 65%;
}

#jafwafxohf .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jafwafxohf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jafwafxohf .gt_indent_1 {
  text-indent: 5px;
}

#jafwafxohf .gt_indent_2 {
  text-indent: 10px;
}

#jafwafxohf .gt_indent_3 {
  text-indent: 15px;
}

#jafwafxohf .gt_indent_4 {
  text-indent: 20px;
}

#jafwafxohf .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_title gt_font_normal" style>Maximum-Liklyhood-Selections QDA-Classifier</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>in Percent [%]</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X1">X1</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X2">X2</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X3">X3</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X4">X4</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X5">X5</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X6">X6</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="X7">X7</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Species">Species</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Prediction">Prediction</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">8.38e-21</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">0.00e+00</td>
<td headers="X6" class="gt_row gt_left">3.88e-290</td>
<td headers="X7" class="gt_row gt_left">1.00e-81</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">9.49e-22</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">0.00e+00</td>
<td headers="X6" class="gt_row gt_left">5.93e-313</td>
<td headers="X7" class="gt_row gt_left">2.32e-94</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">7.02e-25</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">0.00e+00</td>
<td headers="X6" class="gt_row gt_left">0.00e+00</td>
<td headers="X7" class="gt_row gt_left">1.36e-91</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">5.96e-26</td>
<td headers="X4" class="gt_row gt_left">1.24e-174</td>
<td headers="X5" class="gt_row gt_left">0.00e+00</td>
<td headers="X6" class="gt_row gt_left">0.00e+00</td>
<td headers="X7" class="gt_row gt_left">2.09e-92</td>
<td headers="Species" class="gt_row gt_right">1</td>
<td headers="Prediction" class="gt_row gt_right">1</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">3.43e-30</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X4" class="gt_row gt_left">6.27e-61</td>
<td headers="X5" class="gt_row gt_left">1.42e-51</td>
<td headers="X6" class="gt_row gt_left">1.43e-37</td>
<td headers="X7" class="gt_row gt_left">5.70e-09</td>
<td headers="Species" class="gt_row gt_right">3</td>
<td headers="Prediction" class="gt_row gt_right">3</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.44e-25</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X4" class="gt_row gt_left">3.24e-22</td>
<td headers="X5" class="gt_row gt_left">4.46e-69</td>
<td headers="X6" class="gt_row gt_left">1.37e-54</td>
<td headers="X7" class="gt_row gt_left">1.26e-12</td>
<td headers="Species" class="gt_row gt_right">3</td>
<td headers="Prediction" class="gt_row gt_right">3</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">2.12e-29</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">2.89e-35</td>
<td headers="X4" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X5" class="gt_row gt_left">5.57e-175</td>
<td headers="X6" class="gt_row gt_left">5.48e-308</td>
<td headers="X7" class="gt_row gt_left">7.65e-38</td>
<td headers="Species" class="gt_row gt_right">4</td>
<td headers="Prediction" class="gt_row gt_right">4</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">2.53e-14</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">1.33e-17</td>
<td headers="X4" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X5" class="gt_row gt_left">4.99e-142</td>
<td headers="X6" class="gt_row gt_left">5.43e-255</td>
<td headers="X7" class="gt_row gt_left">1.06e-32</td>
<td headers="Species" class="gt_row gt_right">4</td>
<td headers="Prediction" class="gt_row gt_right">4</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">9.70e-68</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">2.44e-11</td>
<td headers="X4" class="gt_row gt_left">1.07e-63</td>
<td headers="X5" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X6" class="gt_row gt_left">3.82e-03</td>
<td headers="X7" class="gt_row gt_left">3.19e-12</td>
<td headers="Species" class="gt_row gt_right">5</td>
<td headers="Prediction" class="gt_row gt_right">5</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">6.19e-80</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">1.57e-48</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">3.90e-269</td>
<td headers="X6" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X7" class="gt_row gt_left">6.91e-14</td>
<td headers="Species" class="gt_row gt_right">6</td>
<td headers="Prediction" class="gt_row gt_right">6</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">7.50e-102</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">8.55e-121</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">0.00e+00</td>
<td headers="X6" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="X7" class="gt_row gt_left">1.53e-25</td>
<td headers="Species" class="gt_row gt_right">6</td>
<td headers="Prediction" class="gt_row gt_right">6</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">2.63e-58</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">4.42e-04</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">1.59e-08</td>
<td headers="X6" class="gt_row gt_left">6.90e-28</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.90e-70</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">3.05e-07</td>
<td headers="X4" class="gt_row gt_left">8.66e-296</td>
<td headers="X5" class="gt_row gt_left">1.48e-10</td>
<td headers="X6" class="gt_row gt_left">8.91e-32</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">5.74e-69</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">2.27e-10</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">2.52e-34</td>
<td headers="X6" class="gt_row gt_left">2.04e-28</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">3.62e-72</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">1.24e-19</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">2.10e-151</td>
<td headers="X6" class="gt_row gt_left">5.61e-57</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
    <tr><td headers="X1" class="gt_row gt_left">1.77e-65</td>
<td headers="X2" class="gt_row gt_left">0.00e+00</td>
<td headers="X3" class="gt_row gt_left">7.02e-27</td>
<td headers="X4" class="gt_row gt_left">0.00e+00</td>
<td headers="X5" class="gt_row gt_left">5.13e-203</td>
<td headers="X6" class="gt_row gt_left">4.20e-48</td>
<td headers="X7" class="gt_row gt_left" style="background-color: #90EE90;">1.00e+02</td>
<td headers="Species" class="gt_row gt_right">7</td>
<td headers="Prediction" class="gt_row gt_right">7</td></tr>
  </tbody>
  
  
</table>
</div>
```

Wie in der folgenden Graphik zu sehen ist, ist die Genauigkeit des 
QDA-Classifiers höher (100%), als die des LDA-Classifiers, welcher eine 
Genauigkeit von 93,75%  aufweist. 


```r
# For LDA classifier
comparision_df_lda <- data.frame(Species = test_data$Species, 
                                 Prediction = predicted_species_lda)

# For QDA classifier
comparision_df_qda <- data.frame(Species = test_data$Species, 
                                 Prediction = predicted_species_qda)

# Create scatter plots for LDA and QDA
plot_lda <- ggplot(comparision_df_lda, aes(x = Species, y = 
                                             Prediction, color = Species)) +
  geom_point() +
  labs(title = "LDA Classifier", x = "Actual Species", y = 
         "Predicted Species") +
  theme_minimal()

plot_qda <- ggplot(comparision_df_qda, aes(x = Species, y = 
                                             Prediction, color = Species)) +
  geom_point() +
  labs(title = "QDA Classifier", x = "Actual Species", y = "Predicted Species") +
  theme_minimal()

data_table <- data.frame("LDA Pred." = predicted_species_lda,   
                         Species = test_data$Species, 
                         "QDA Pred." = predicted_species_qda)

table_theme <- ttheme_minimal(
  core = list(fg_params = list(hjust = 0, x = 0.1)),
  colhead = list(fg_params = list(hjust = 0, x = 0.1))
)

# Convert the data frame to a tableGrob
table_grob <- tableGrob(data_table,  theme = tt2, rows = NULL)
# Adjust the table size

# Display plots side by side with the table of Species and Predictions
grid.arrange(plot_lda, table_grob, plot_qda, ncol = 3)
```

<img src="Fish-Classifier_files/figure-html/classifieing comparison-1.png" style="display: block; margin: auto;" />




# Quellen

[1] Vorlesungsscript Datenerfassung in Umweltinformationssystemen

[2] Trevor Hastie Robert Tibshirani Jerome Friedman. The Elements of 
Statistical Learning Data Mining, Inference, and Prediction. 
Springer Series in Statistics. 2009

[3] Scikit-learn. Linear and Quadratic Discriminant Analysis. Zugriffszeit: 08.05.2023 17:45 Uhr. https://scikit-learn.org/stable/modules/lda_qda.html

# Packages

Es wurden folgende Packages verwendet:

tidyverse  
ggplot2    
zoo  
scales  
lubridate   
mvtnorm  
caret  
grid  
gridExtra  
gt  



