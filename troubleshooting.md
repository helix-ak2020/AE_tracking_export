In this document we list some issues with our script. 

1. "null is not an object" 
  This error occurs often when trying to export tracking data, we know of two cases when it is caused:
  a) The language of After Effects is not set to english 
    Unfortunately, some of the variable names in after effects are differ with different language settings. Therefore our script cannot locate
    the tracking points. Fortunately, it is quite easy to change the language:
    Just rename the following file:
    c:\Program Files\Adobe\Adobe After Effects CC 2015\Support Files\zdictionaries\after_effects_de.dat
    to something like: after_effects_de.dat.ORI.dat 
    The location of this file might differ between After Effects Versions. 
    (Source & more Info (German) : https://jkdigital.de/after-effects-cc-2015-englisch/ )
    
    You might need to create a new project afterwards, as the variable names are saved locally for the project. As an alternative for German 
    to Englisch you can manually change the "Track-Punkt" to "Track Point". But you would need to do that for every tracker individually, 
    so changing the language is probably easier.
    
  b) The footage layer has not been renamed 
    Our script is asking for the name of the layer where your footage is located inside the composition. the default is "Data". 
    If you don't care about the layer name you can always name it "Data". If you need a specific name, set the layer to it, and type it
    into the popup  window when running our script. If the two names are different, you'll get an error message. If you can't change the layer
    name, the display in the composition timeline might be set to "source name" in this case, simply click on "source name" to switch it to 
    "layer name". Then you can rename your layer
    
    
 This will be expanded as more issues are found.... 
 Please notify us if you experience any issues with our script. 
    
     
    
