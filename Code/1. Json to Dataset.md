**pre:**
 arquivos em formatação [Json](http://nitanilla.com/svm-issues/ml-queries/). São 55 queries, cada query numa pasta.
Cada pasta contem 10 json files. Em cada json file tem se 100 issues

**argumentações:**
1. se abre a pasta ml-queries que contem 55 pastas 
2. se entra em cada pasta e se percorre nos 10 files. Em cada file se evalua o seguinte:

- em cada file se verifica se tem issues de tipo pull-request, se tiver se retiram.
- se agregam colunas para cada tipo de label: bug, enhancement, question, wontfix, duplicated, invalid
- se marca cada coluna de label segum a classifição. A classificação de labels verifica labels criadas por usuarios. ex: "type-enhancement" e nao somente aquelas proprias do GitHub

**pos**
um dataset de issues chamado [ds.imasmari.github](http://nitanilla.com/svm-issues/passo1/ds.imasmari.github.Rda) contendo os seguintes atributos: 

- 	id: GitHub issue id
- 	state:  se uma issue esta aberta o fechada
- 	titulo: titulo da issue
- 	body: descrição da issue
- 	created: data de criação
- 	updated: ultima data de modificação
- 	url: link para acessar à issue
- 	omments: o numero de comentarios de uma issue
- 	score: o valor de relevanca que GitHub da a uma issue
- 	user: o propietario do projeto de uma issue
- 	project: a url do projeto que contem a issue
- 	query: a query utilizada para recuperar dita issue
- 	bug
- 	enhancement
- 	question
- 	wontfix
- 	duplicated
- 	invalid


```R
library(tm)
library(jsonlite)
root<-"C:/Users/Roxana/Dropbox/svm-issues/ml-queries"
setwd(root)
files<-list.dirs(path=".", full.names=TRUE)
for (j in 2:length(files)){
	setwd(files[j])

	for (h in 1:10){
		filename<-paste("issues-page",h,".json",sep="")
		jsonFile<-fromJSON(txt=filename)
		queryFile<-gsub("./ml-","",files[j])
		rawDataset<-data.frame(id=jsonFile$items$id, 
			state=jsonFile$items$state,
			title=jsonFile$items$title, 
			body=jsonFile$items$body, 
			created=jsonFile$items$created_at,
			updated=jsonFile$items$updated_at, 
			url=jsonFile$items$html_url,
			comments=jsonFile$items$comments,
			score=jsonFile$items$score,
			user=jsonFile$items$user$login,
			project=jsonFile$items$repository_url,
			query=queryFile,
			stringsAsFactors=F
		) 
		if (is.null(jsonFile$items$pull_request)!=TRUE) { # si existe pull-request
			addcolumn<-c("pullrequest")
			rawDataset[,addcolumn]<-jsonFile$items$pull_request$url
			#Todo o que é pullrequest tem algum valor, o que é issue tem NA
			#filtramos só as issues verificando se o valor de pullrequest é NA 
			discardPullR<-subset(rawDataset, is.na(rawDataset$pullrequest))
			#recreamos o dataset retirando a coluna pullrequest
			keeps<-c("id", "state", "title", "body", "created", "updated", "url",
			"comments", "score", "user", "project", "query")
			only.issues<-discardPullR[keeps]
		} else {
			only.issues<-rawDataset
		}

		#adicionar colunas para classificar as issues
		addcolumn<-c("enhancement")
		only.issues[,addcolumn]<-0
		addcolumn<-c("bug")
		only.issues[,addcolumn]<-0
		addcolumn<-c("duplicate")
		only.issues[,addcolumn]<-0
		addcolumn<-c("helpwanted")
		only.issues[,addcolumn]<-0
		addcolumn<-c("invalid")
		only.issues[,addcolumn]<-0
		addcolumn<-c("wontfix")
		only.issues[,addcolumn]<-0
		addcolumn<-c("question")
		only.issues[,addcolumn]<-0

		#tomar os numero de indices para nao perder o rastro quando se comece a classificar
		idx.only.issues<-as.integer(row.names(only.issues))

		#acessar a todos os labels do only.issues
		itemsWithLabel<-jsonFile$items[idx.only.issues,]
		for (i in 1:nrow(itemsWithLabel)){
			setLabels<-itemsWithLabel$labels[[i]]
			#para cada tipo de label
			isEnhancement<-grep("enhancement", tolower(setLabels$name))
			if (isTRUE(isEnhancement!=0)) {only.issues$enhancement[i]<-1}
			isBug<-grep("bug", tolower(setLabels$name))
			if (isTRUE(isBug!=0)) {only.issues$bug[i]<-1}
			isDuplicate<-grep("duplicate", tolower(setLabels$name))
			if (isTRUE(isDuplicate!=0)) {only.issues$duplicate[i]<-1}
			isHelpWanted<-intersect(grep("help", tolower(setLabels$name)),grep("wanted", tolower(setLabels$name)))
			if (isTRUE(isHelpWanted!=0)) {only.issues$helpwanted[i]<-1}
			isInvalid<-grep("invalid", tolower(setLabels$name))
			if (isTRUE(isInvalid!=0)) {only.issues$invalid[i]<-1}
			isWontfix<-grep("wontfix", tolower(setLabels$name))
			if (isTRUE(isWontfix!=0)) {only.issues$wontfix[i]<-1}
			isQuestion<-grep("question", tolower(setLabels$name))
			if (isTRUE(isQuestion!=0)) {only.issues$question[i]<-1}
		}
		if (h==1){
			classified.issues<-only.issues
		}else{
			classified.issues<-rbind(classified.issues, only.issues)
		}
	}

if (j==2){
	ds.imasmari.github<-classified.issues
} else{
	ds.imasmari.github<-rbind(ds.imasmari.github, classified.issues)
}

root<-root<-"C:/Users/Roxana/Dropbox/svm-issues/ml-queries"
setwd(root)

}

rownames(ds.imasmari.github) <- c(1:length(ds.imasmari.github$id))

```
