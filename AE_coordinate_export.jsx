//Try enclosure to catch any error that might occur
try{    
    //Check if a composition has been selected and if yes save is as variable comp
    if(app.project.activeItem != null){
        var comp = app.project.activeItem;
    }else{
        throw new Error("No Composition selected");  
    }
    //Ask for the name of the layer containing the trackers
    var layerN = prompt("Please type in the name of the layer", "Data");
    //Compute the number of frames in the comp
    var layer = comp.layer(layerN);
    
    var frames = (layer.outPoint-layer.inPoint)/comp.frameDuration;
    //Get the number of trackers in the layer
    var tracker = comp.layer(layerN).motionTracker.numProperties;
    //Initialize array to collect results from each tracker
    var result = new Array(tracker);
    

    for(j=0; j<tracker; j++){
        //Reset comp time to start
        comp.time = layer.inPoint;	
        //Initialize temporary array with coordinates for each frame
        var temp = new Array();
        //Build name of current tracker as text
        var trackerN = "Tracker " + (j+1);
        for(i=0; i<frames; i++){
            //Get x coordinate from tracker
            var x = comp.layer(layerN).motionTracker(trackerN)("Track Point 1").attachPoint.value[0];
            //Get y coordinate from tracker
            var y = comp.layer(layerN).motionTracker(trackerN)("Track Point 1").attachPoint.value[1];
            //Combine coordinates into point
            var point = [x, y];
            //Save point to temporary array
            temp[i] = point
            //Go to next frame
            comp.time+=comp.frameDuration;
        }
        //Put temporary array with results from current tracker into result array
        result[j] = temp;
    }

    //Get script path
    var scriptFolderPath = File($.fileName).path;
    //Build path of output folder
    var TimerFolderPath = new Folder (scriptFolderPath + "/Output/" +comp.name);
    //Check if the output folder already exists
    if(TimerFolderPath.exists)
        throw new Error("Folder already exists");
    //Create output folder
    TimerFolderPath.create()
   
    for(j=0; j<tracker; j++){
        //Build file name out of tracker number and date + time
        var fileName = "/Tracker" + (j+1) + ".txt";
        //Initialize File and assign location
        var JFile = new File(TimerFolderPath.fullName + encodeURI(fileName));
        //Call write function
        writeFile(JFile, j);  
    }
    alert("Done!");
}catch(e){
    //Throw error message
    alert(e);
}
      
function writeFile(fileObj, tracker) {  
    //Specify file encoding
    encoding = "utf-8"; 
    //Initialize File
    fileObj = (fileObj instanceof File) ? fileObj : new File(fileObj);           
    //Define file encoding
    fileObj.encoding = encoding; 
    //Start writing
    fileObj.open("w"); 
    //Write column names as first line
       fileObj.writeln("x,y");
    for(i=0; i<frames; i++){
        //Write line consisting of x and y coordinate of point
        fileObj.writeln(result[j][i][0] + "," + result[j][i][1]);
    }
    //Stop writing and save
    fileObj.close();
    return fileObj;
}
