
// add plots as objects to map
Map.addLayer(plots, {color: 'red'}, 'Plot Locations');
Map.centerObject(plots, 8);

// import MODIS EVI data
var modisEVI = ee.ImageCollection("MODIS/061/MOD13Q1").select('EVI');

// define years to analyze
var years = ee.List.sequence(2000, 2025);

// function to analyze each year
var analyzeYear = function(year) {
  // convert year to number
  year = ee.Number(year);
  
  // define May-July date range for the specific year
  var startDate = ee.Date.fromYMD(year, 5, 1);
  var endDate = ee.Date.fromYMD(year, 7, 31);
  
  // filter MODIS data to May-July period
  var seasonalEVI = modisEVI.filterDate(startDate, endDate);
  
  // check if we have any images for this period
  var imageCount = seasonalEVI.size();
  
  // create a reducer to get mean EVI value
  var reducer = ee.Reducer.mean().setOutputs(['mean_evi']);
  
  // function to process when we have images
  var processWithImages = function() {
    // create a mosaic of all images in the period
    var evi = seasonalEVI.mosaic();
    
    // extract values at plot locations
    var sampledPoints = evi.reduceRegions({
      collection: plots,
      reducer: reducer,
      scale: 250
    });
    
    // count plots with valid data (not null)
    var validPlots = sampledPoints.filter(ee.Filter.notNull(['mean_evi']));
    
    return ee.Feature(null, {
      'year': year,
      'plot_count': validPlots.size(),
      'mean_evi': validPlots.aggregate_mean('mean_evi')
    });
  };
  
  // function to handle when no images are available
  var processWithoutImages = function() {
    return ee.Feature(null, {
      'year': year,
      'plot_count': 0,
      'mean_evi': 0
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
print('Results by year (May-July):', results);

// export results to CSV
Export.table.toDrive({
  collection: results,
  description: 'MODIS_EVI_May_July_Plot_Counts',
  fileFormat: 'CSV'
});