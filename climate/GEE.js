// 读取 Assets 中的坐标数据
var pointFeatures = ee.FeatureCollection('projects/ee-1239331448/assets/vvv');

// 定义时间范围
var startDate = '1950-01-01';
var endDate = '2015-12-31';

// 选择 MOD11A1 数据集并应用时间过滤
var dataset = ee.ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR')
                .filterDate(startDate, endDate);

// 定义一个函数来提取特定点的温度、降水和日期
function getClimateData(feature) {
  // 映射函数，为每个图像提取温度、降水和日期
  function mapFunction(image) {
    var climate = image.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: feature.geometry(),
      scale: 1000
    });

    // 获取图像的日期
    var date = image.date().format('YYYY-MM-dd');

    return ee.Feature(null, {
      'PEP_ID': feature.get('PEP_ID'),
      'temperature': climate.get('temperature_2m'),
      'precipitation': climate.get('total_precipitation_sum'),
      'evaporation': climate.get('potential_evaporation_sum'),
      'thermal': climate.get('surface_thermal_radiation_downwards_sum'),
      'solar': climate.get('surface_solar_radiation_downwards_sum'),
      'date': date
    });
  }

  // 为每个图像应用映射函数
  var climateOverTime = dataset.map(mapFunction);

  return climateOverTime;
}

// 应用函数并整合结果
var climateTimeSeries = pointFeatures.map(getClimateData).flatten();

// 导出结果到 Google Drive
Export.table.toDrive({
  collection: climateTimeSeries,
  description: 'ERA5_land_climate_data',
  fileFormat: 'CSV',
  selectors: ['PEP_ID', 'temperature', 'precipitation', 'evaporation','thermal','solar','date',] // 指定要导出的字段
});
