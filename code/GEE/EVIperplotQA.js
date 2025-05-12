// import MODIS EVI data collection
var modisCollection = ee.ImageCollection("MODIS/061/MOD13Q1");

// define years to analyze (2000-2024)
var years = ee.List.sequence(2000, 2024);

// function to extract quality bits from the QA band
function extractBits(image, start, end) {
  var pattern = 0;
  for (var i = start; i <= end; i++) {
    pattern += Math.pow(2, i);
  }
  // return a single band image of the extracted QA bits
  return image.select(['SummaryQA']).bitwiseAnd(pattern).rightShift(start);
}

// function to mask poor quality pixels
function maskQuality(image) {
  var qa = image.select('SummaryQA');
  
  // MODIS summary QA values:
  // 0: Good data, use with confidence
  // 1: Marginal data, useful but look at detailed QA for more info
  // 2: Snow/ice, pixel covered with snow/ice
  // 3: Cloudy, pixel not useful
  
  // create a mask
  var qualityMask = qa.eq(0);
  
  // apply the mask to the image
  return image.updateMask(qualityMask);
}

// function to calculate mean EVI per plot during May-July
var calculateMeanEVI = function(year) {
  year = ee.Number(year);
  
  // define May-July date range
  var startDate = ee.Date.fromYMD(year, 5, 1);
  var endDate = ee.Date.fromYMD(year, 7, 31);
  
  // filter MODIS data to the specific year
  var seasonalEVI = modisCollection
    .filterDate(startDate, endDate)
    .map(maskQuality)  // apply quality filtering
    .select('EVI');
  
  // check if we have any images for this period
  var imageCount = seasonalEVI.size();
  
  // function to process when we have images
  var processWithImages = function() {
    // calc mean EVI for the period
    var meanEVI = seasonalEVI.mean();
    
    // extract values at plot locations
    var sampledPoints = meanEVI.reduceRegions({
      collection: plots,
      reducer: ee.Reducer.mean(),
      scale: 250  // MODIS EVI resolution is 250m
    });
    
    // add year and image count as properties to each feature
    sampledPoints = sampledPoints.map(function(feature) {
      return feature.set({
        'year': year,
        'image_count': imageCount,
        'data_quality': 'filtered'
      });
    });
    
    return sampledPoints;
  };
  
  // function to handle when no images are available
  var processWithoutImages = function() {
    // return the original plots with null EVI values and year property
    return plots.map(function(feature) {
      return feature.set({
        'mean': null,
        'year': year,
        'image_count': 0,
        'data_quality': 'no_data'
      });
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
  description: 'MODIS_EVI_QA_' + Date.now(),
  fileFormat: 'CSV'
});