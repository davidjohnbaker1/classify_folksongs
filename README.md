# Classifying Folksongs Project

## Overview

This project is an investigation whether or not extracted featuers of melodies can be used to train some sort of ML classifier.
The idea is to explore a bit with it and then write it up into a small paper for the Journal of New Music Research.
The big goals of the project are

* Get small publication in JNMR
* Learn about various ML classification methods 

## Tasks 

### Current

* [X] Redownload Essen Collection with All Countries
* [X] Rerun Feature Extraction Script
* [ ] Get American Folk Songs from Kodaly
* [ ] Get Meertens Dutsch Folk Song (Peter Von Kronnerberg?) 
* [ ] Convert RS200 Melody to Krn

### Literature Review

* [X] Start Overleaf with JNMR Requirements on page
	LINK FOR JNMR http://www.tandfonline.com/action/authorSubmission?journalCode=nnmr20&page=instructions
* [ ] Find 15 Sources related to Essen Folk song and categorization (aka cited by Shanahan NA paper) 
* [ ] Write Outline of narrative based on previous research
* [ ] Clearly state what the goals and implications of the research will be

### Methods

#### Data

* [X]  Get Essen Collection Features 6210
* [ ] Get American Features http://kodaly.hnu.edu/collection.cfm 522
* [X] Chinese Features?
* [X] Write R Script to Add in More Humdrum Metadata (Like German Region) 
* [X] Get Complete Densmore 

#### Analysis 

* [X] Create Cleaning Script of Extracted Features
* [ ] Write rationale why ML method of choice will be used in accordance with Hypotheses
* [ ] Create charts based on classification findings 

### Discussion 

* [ ] Wait until Introduction and Methods have been written.

## Project Log

1. Project Started: Fri Dec 22 11:27:18 2017
	* Wrote Markdown File Outline 
	* Started Project Files, Git, Ball rolling
2. Return after break Wed Jan 10 18:04:57 2017
	* Read Toiviainen and Eerola 2001 (Very basic attempt at neural net to show similarity across 6 features)
	* Spent almost entire day waiting for loops and fixing problems with batch convert of essen AND asia features
	* Converted densmore collection after dinner
3. Added in ESAC region information Thu Jan 11 13:04:30 
	* Added R code to add in region information for essen
	* Also created columns for subsetting of Densmore Collections
4. Emailed Kodaly Center again, set up outline for paper 
5. Started Exploratory Script Friday Jan 19 11:45:30
	* Get full Essen from DS
	* Talk ML techniques w DS meeting, NN, KNN, Decision Tree
	* Recursive 
