## Introduction
Lexsub-scala was used to obtain results of *Language Transfer Learning for Supervised Lexical Substitution (Hintz & Biemann, 2016)* [[pdf]](http://aclweb.org/anthology/P/P16/P16-1012.pdf) [[bib]](http://aclweb.org/anthology/P/P16/P16-1012.bib)


## Running
Lexsub-scala can be run on Windows, MacOS and Linux (tested on Windows 7, MacOS Sierra, Ubuntu 15.04)

1. Install pre-requisites
	* Java >= 1.7.0
	* Maven >= 3.2.5
	* Cygwin (when running on Windows)
	
	When manually building data-dependencies, further requirements are
	* [Python 2.7](https://www.python.org/download/releases/2.7/) + Python dependencies
	* [JobimText](https://www.lt.informatik.tu-darmstadt.de/de/software/jobimtext/)
	* [word2vec](https://code.google.com/archive/p/word2vec/)

2. Obtain and compile the dependency ["scala-utilities"](https://github.com/hintz/scala-utilities) into your local maven repository
	```
	git clone https://github.com/hintz/scala-utilities.git && cd scala-utilities
	mvn install
	```
	
3. Obtain and package lexsub-scala (this repository)
	```
	git clone https://github.com/hintz/lexsub-scala.git && cd lexsub-scala
	```
	
	Install a custom build of RankLib with some bugfixes from the ```./local-repo``` folder
	```
	cd local-repo && ./install.sh && cd ..
	```
	
	Package this project
	```
	mvn package
	```
	
	This creates a [shaded jar](https://maven.apache.org/plugins/maven-shade-plugin/) with all dependencies.
	
	Please note that you should setup a Maven repository that has access to the following artifacts in your user settings ```~/.m2/settings.xml``` (Syntax of this list: ```group: artifact (version)```)
	```
	org.scala-lang: scala-actors (2.11)
	org.scala-lang: scala-library (2.11)
	org.scala-lang: scala-xml (2.11.0-M4)
	org.scalaz: scalaz-core_2.11 (7.1.0)
	org.scalanlp: breeze_2.11 (0.11)
	ciir.umass.edu: ciir.umass.edu.RankLib (2.6.0)
	com.googlecode.jweb1t: com.googlecode.jweb1t (1.3.0)
	com.googlecode.mate-tools: anna (3.61)
	com.h2database: h2 (1.3.176)
	de.tudarmstadt.langtech: scala-utilities (0.1.6)
	de.tudarmstadt.ukp.dkpro.core: de.tudarmstadt.ukp.dkpro.core.io.web1t-asl (1.7.0)
	de.tudarmstadt.ukp.uby: de.tudarmstadt.ukp.uby.lmf.api-asl (0.6.0)
	de.tudarmstadt.ukp.uby: de.tudarmstadt.ukp.uby.lmf.model-asl (0.6.0)
	de.tuebingen.uni.sfs.germanet: germanetapi (9.0.1)
	edu.stanford.nlp: corenlp (1.0.4.20110605)
	org.apache.opennlp: opennlp-tools (1.5.3)
	org.cleartk: cleartk-ml-mallet (1.2.2)
	org.maltparser: maltparser (1.8.1)
	org.yaml: snakeyaml (1.8)
	junit: junit (4.11)
	```
	
5. Obtain data dependencies
	Some of these files were crawled and/or precomputed using Python scripts in the ```/scripts``` folder as well as Scala scripts in the packages ```de.tudarmstadt.langtech.lexsub_scala.run.{semeval2007,evalita2009,germeval2015}.setup.*```.

	* Lexsub task data
		* [SemEval-2007: Lexsub](http://nlp.cs.swarthmore.edu/semeval/tasks/task10/summary.shtml) (English)
		* [Evalita-2009: Lexsub](http://www.evalita.it/2009/tasks/lexical) (Italian)
		* [Germeval-2015: Lexsub](https://sites.google.com/site/germeval2015/) (German)
		
	* Lexical resources
		* [WordNet 3.0](https://wordnet.princeton.edu/)
		* [GermaNet](http://www.sfs.uni-tuebingen.de/GermaNet/)
		* [MultiWordNet](http://multiwordnet.fbk.eu/english/home.php)
		* [TWSI](https://www.inf.uni-hamburg.de/en/inst/ab/lt/resources/software/twsi-substituter.html)
		
	* N-gram counts
		* [Web1T](https://catalog.ldc.upenn.edu/ldc2006t13) (English)
		* [Web1T](https://catalog.ldc.upenn.edu/ldc2006t13) (German)
		* [Web1T](https://catalog.ldc.upenn.edu/ldc2006t13) (Italian)
	
	* Word embeddings (resp. word vector + context vector files)
		* English word2vec embeddings by [(Melamud, 2015)](https://github.com/orenmel/lexsub)
		* Italian word2vec embeddings -> running  [word2vec](https://code.google.com/archive/p/word2vec/) on [itWac](https://www.sketchengine.co.uk/itwac-corpus/)
		* German word2vec embeddings -> running [word2vec](https://code.google.com/archive/p/word2vec/) on [LCC: Newswire Corpus](http://corpora.uni-leipzig.de/en?corpusId=deu_newscrawl_2011)
	
	* Distributed Thesaurus (DT) files 
		* English first order trigram DT
		* English second order trigram DT
		* German first order trigram DT
		* German second order trigram DT
		* Italian first order trigram DT
		* Italian second order trigram DT
		
		Trigram corpus dependencies: The following corpora were used to obtain these DTs using the [JobimText](https://www.lt.informatik.tu-darmstadt.de/de/software/jobimtext/) toolkit:
		
		* Italian: [itWac](https://www.sketchengine.co.uk/itwac-corpus/)
		* German: [LCC: Newswire Corpus](http://corpora.uni-leipzig.de/en?corpusId=deu_newscrawl_2011)
		* English: [Gigaword](https://catalog.ldc.upenn.edu/ldc2003t05) + [LCC](http://wortschatz.uni-leipzig.de/en/download)
		
	* Co-occurence counts
		Statically computed co-occurence counts based on SemEval datasets and above corpora, using ```scripts/convertCoocs.py```
		* semeval2007-coocs
		* evalita2009-coocs
		* germeval2015-coocs

	* Preprocessing models
		* OpenNLP [POS tagger (english, perceptron)](http://opennlp.sourceforge.net/models-1.5/en-pos-perceptron.bin)
		* OpenNLP [POS tagger (german, perceptron)](http://opennlp.sourceforge.net/models-1.5/de-pos-perceptron.bin)
		* OpenNLP [Tokenizer (english)](http://opennlp.sourceforge.net/models-1.5/en-token.bin)
		* OpenNLP [Tokenizer (german)](http://opennlp.sourceforge.net/models-1.5/de-token.bin)
		* Mate tagger (italian) -> custom-trained using [mate-tools](https://code.google.com/archive/p/mate-tools/)
		* Mate lemmatizer (italian) -> custom-trained using [mate-tools](https://code.google.com/archive/p/mate-tools/)
		* Mate parser (italian) -> custom-trained using [mate-tools](https://code.google.com/archive/p/mate-tools/)
	
6. Adapt the settings files ```*path.yaml``` to match data location (or copy resources to the corresponding paths)
	* ```crosstraining-paths.yaml```
	* ```evalita2009-paths.yaml```
	* ```germeval2015-paths.yaml```
	* ```semeval2007-paths.yaml```
	* ```twsi-paths.yaml```
	
7. The files ```run*.sh``` contain different evaluation runs
	* ```run-crosstraining.sh``` computes the primary results for **transfer learning**
	* ```run-ablation.sh``` runs a single ablation experiment across all languages
	* ```run-all-ablations.sh``` runs all ablation experiments
	
Note that most intermediate results will be cached into files in the ```./cache``` folder. The first run will take substantially longer to build these lookup files (80+ GB disk space required).

### Results
Results will be written to stdout.