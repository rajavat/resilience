// add plots as objects to map
Map.addLayer(plots, {color: 'red'}, 'Plot Locations');
Map.centerObject(plots, 8);

// import MODIS NDVI data
var modisNDVI = ee.ImageCollection("MODIS/061/MOD13Q1").select('NDVI');

// define years to analyze
var years = ee.List.sequence(2000, 2025);

// function to analyze each year
var analyzeYear = function(year) {
  year = ee.Number(year);
  
  var startDate = ee.Date.fromYMD(year, 5, 1);
  var endDate = ee.Date.fromYMD(year, 7, 31);
  
  var seasonalNDVI = modisNDVI.filterDate(startDate, endDate);
  
  var imageCount = seasonalNDVI.size();
  
  var processWithImages = function() {
    // create a mosaic of all images in the period
    var ndvi = seasonalNDVI.mosaic();
    
    // extract values at plot locations
    var sampledPoints = ndvi.reduceRegions({
      collection: plots,
      reducer: ee.Reducer.count(),
      scale: 250
    });
    
    // count plots with valid data (not null)
    var validPlots = sampledPoints.filter(ee.Filter.notNull(['count']));
    
    return ee.Feature(null, {
      'year': year,
      'plot_count': validPlots.size(),
      'image_count': imageCount
    });
  };
  
  // function to handle when no images are available
  var processWithoutImages = function() {
    return ee.Feature(null, {
      'year': year,
      'plot_count': 0,
      'image_count': 0
    });
  };
  
  // use conditional to handle years with no data
  return ee.Algorithms.If(
    imageCount.gt(0),
    processWithImages(),
    processWithoutImages()
  );
};

// map the analysis function over all years
var results = ee.FeatureCollection(years.map(analyzeYear));

// print results to console
print('NDVI Plot Counts by Year (May-July):', results);

// export results to CSV
Export.table.toDrive({
  collection: results,
  description: 'MODIS_NDVI_Plot_Counts',
  fileFormat: 'CSV'
});
