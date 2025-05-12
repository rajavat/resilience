var modisNDVI = ee.ImageCollection("MODIS/061/MOD13Q1")
  .select('NDVI');

// calculate mean NDVI from may-july for each year
function calculateMeanNDVI(year) {
  var startDate = ee.Date.fromYMD(year, 5, 1);
  var endDate = ee.Date.fromYMD(year, 7, 31);
  
  var ndviCollection = modisNDVI
    .filterDate(startDate, endDate);
  
  var meanNDVI = ndviCollection.mean();
  
  return meanNDVI.set('year', year);
}

// map this over 2000-2024
var years = ee.List.sequence(2000, 2024);
var annualNDVI = ee.ImageCollection(years.map(calculateMeanNDVI));

// extract NDVI vals for each plot
function extractNDVIForPlots(image) {
  var year = image.get('year');
  var ndviValues = image.reduceRegions({
    collection: plots,
    reducer: ee.Reducer.mean(),
    scale: 250
  });
  return ndviValues.map(function(feature) {
    return feature.set('year', year);
  });
}

// apply this to the annual NDVI collection
var plotNDVIValues = annualNDVI.map(extractNDVIForPlots).flatten();

Export.table.toDrive({
  collection: plotNDVIValues,
  description: 'MODIS_NDVI_' + Date.now(),
  fileFormat: 'CSV'
});
