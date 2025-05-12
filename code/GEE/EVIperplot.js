Map.addLayer(plots, {color: 'red'}, 'Plot Locations');
Map.centerObject(plots, 8);

// import MODIS EVI data collection
var modisEVI = ee.ImageCollection("MODIS/061/MOD13Q1")
  .select('EVI');  // Select only the EVI band

// define years to analyze (2000-2024)
var years = ee.List.sequence(2000, 2024);

// function to calculate mean EVI for each plot during May-July for a specific year
var calculateMeanEVI = function(year) {
  year = ee.Number(year);
  
  // define May-July date range for the specific year
  var startDate = ee.Date.fromYMD(year, 5, 1);
  var endDate = ee.Date.fromYMD(year, 7, 31);
  
  // filter MODIS data to May-July period for the specific year
  var seasonalEVI = modisEVI.filterDate(startDate, endDate);
  var imageCount = seasonalEVI.size();
  
  // function to process when we have images
  var processWithImages = function() {
    // calculate mean EVI for the period
    var meanEVI = seasonalEVI.mean();
    
    // extract values at plot locations
    var sampledPoints = meanEVI.reduceRegions({
      collection: plots,
      reducer: ee.Reducer.mean(),
      scale: 250  // MODIS EVI resolution is 250m
    });
    
    // add year as a property to each feature
    sampledPoints = sampledPoints.map(function(feature) {
      return feature.set('year', year);
    });
    
    return sampledPoints;
  };
  
  // function to handle when no images are available
  var processWithoutImages = function() {
    // return the original plots with null EVI values and year property
    return plots.map(function(feature) {
      return feature.set('mean', null)
                   .set('year', year);
    });
  };
  
  // use conditional to handle years with no data
  return ee.FeatureCollection(ee.Algorithms.If(
    imageCount.gt(0),
    processWithImages(),
    processWithoutImages()
  ));
};

// map the function over all years
var allResults = ee.FeatureCollection(years.map(calculateMeanEVI)).flatten();

// print a sample of the results
print('Sample of results:', allResults.limit(10));

// export results to CSV
Export.table.toDrive({
  collection: allResults,
  description: 'MODIS_EVI_' + Date.now(),
  fileFormat: 'CSV'
});


var year2023 = ee.Date.fromYMD(2023, 5, 1);
var endDate2023 = ee.Date.fromYMD(2023, 7, 31);
var evi2023 = modisEVI.filterDate(year2023, endDate2023).mean();

Map.addLayer(evi2023, 
  {min: 0, max: 8000, palette: ['FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901', '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01', '012E01', '011D01', '011301']}, 
  'Mean EVI May-July 2023');
  
Map.addLayer(plots, {color: 'red'}, 'Plot Locations');
Map.centerObject(plots, 8);