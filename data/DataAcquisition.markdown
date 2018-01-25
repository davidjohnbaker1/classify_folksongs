# Data Acquisition

This document provides links and how to of data acquisition process.

## Essen Collection

Essen collection was downloaded on Jan 10, 2018 from http://kern.ccarh.org/browse?l=essen.
Click on little 'z' for all files in zip format. 

This produced 8473 files
```find . -name \*.krn | wc```
Then I moved them to essenbin with
 ```find . -name \*.krn -exec cp {} ../essen\_bin/ \;```
Then run bash loop to convert to xml (hum2xml)
 ```for i in *.krn; do; hum2xml > $i; done```
 ```rename 's/\.krn\.xml$/.krn/' *.xml```
Then use Musescore to convert in batch. Tons of problms here thinking some are not xml right
Now nun melconv
```ls \*.mid | melconv -f csv -s -I```
Then run fantastic 
```essenFeatures \<\- compute.features(melody.filenames = list.files(pattern=".csv"), dir = ".",use.segmentation = FALSE,write.out = TRUE)```

## Densmore Collection 

Got densmore_complete from DS.

```find . -name \*.krn | wc```
Then I moved them to densmorebin with
 ```find . -name \*.krn -exec cp {} ../densmore\_bin/ \;```
Then run bash loop to convert to xml (hum2xml)
 ```for i in *.krn; do; hum2xml > $i.xml; done```
 ```rename 's/\.krn\.xml$/.xml/' *.xml```
Then use Musescore to convert in batch. Minmal problems in comparison
Now nun melconv
```ls \*.mid | melconv -f csv -s -I```
Then run fantastic after changing working directory 
```densmoreFeatuers <- compute.features(melody.filenames = list.files(pattern=".csv"), dir = ".",use.segmentation = FALSE,write.out = TRUE)```

## American Kodlay Features
