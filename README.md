# ZoteroRnalysis
Analyze your Zotero library of references with R, to produce graphics and csv files

- Everything you need to know is self-contained in the `ZoteroRnalysis.R` file.
- The file `My library.csv` is an example of Zotero references to be processed.
- The file `journal_titles_reconciled.csv` is an example of reconciled data with OpenRefine.
- The R program will produce a `output` subfolder with `.pdf` and `.csv` files

## Examples of analysis : 
- Distribution of the journal articles, by years of publication
- Journal names, counted and ranked
- Book publisher names, counted and ranked 
- Author names, counted and ranked
- Languages used, counted and ranked
- Number of authors per papers, distributed by year
- Personal tags used, counted, ranked and distributed by year (two levels of tags)
- For each article, what is the time span between the journal inception year and the article publication year ? (with reconciled data from Wikidata with OpenRefine)
- Distribution of the countries of origin of the journals  (with reconciled data from Wikidata with OpenRefine)

View sample graphics visualizations here : https://jdr.hypotheses.org/1907

![Examples](https://github.com/pmartinolli/ZoteroRnalysis/blob/main/GitHub-illustration.png)

## Credits & Acknowledgments

- Author : Pascal Martinolli
- Date (v1) : 2023-12-01
- Thanks to :
  - ChatGPT 3.5 by OpenAI for a lot of help with back and forth feedbacks on my R code
  - Caroline Patenaude, Data librarian at Université de Montréal for teaching me R & OpenRefine
  - Céline Van den Rul at https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a for word clouds
  - David Tingle at https://davidtingle.com/misc/bib for ideas of analysis to perform
  - Zotero development team
  - R and R Studio development team
  - OpenRefine development team
  - Wikidata development team and community of contributors

GPL-3.0 license https://github.com/pmartinolli/ZoteroRnalysis/blob/main/LICENSE 

Last version of the code available at https://github.com/pmartinolli/ZoteroRnalysis/
