# HaskellMLRecognition
This is my attempt at firstly, building a simple ML Decision Tree classifier library and then (if I have time) utilising it to build a basic handwriting recognition tool. The whole project is to be completed from scratch by myself and all in Haskell. I will be using ID3 to build the decision trees.

## Issues
  * **Predicting with new attribute values** An entry with a new attribute value could still be very easy to predict, for instance an animal with 8 legs, 8 eyes but is coloured pink, is still likely to be a spider... Even if the model has never seen a pink spider before. To get around this at the moment, if 'predict' comes across a new attribute value, it removes it and lets the children vote as seperate decision trees on the remaining attributes. This is not ideal because all the spiders might be in the 'black' child of our previous example, and our new data entry might fit this category perfectly, but this tree carries no more weight in the vote than any of the other children trees... May look into weighting votes but this would require the tree to remember far more information.