import cdsapi

c = cdsapi.Client()

c.retrieve(
    'cams-global-reanalysis-eac4-monthly',
    {
        'format': 'netcdf',
        'variable': [
            'particulate_matter_10um', 'particulate_matter_2.5um', 'total_column_carbon_monoxide',
            'total_column_nitrogen_dioxide', 'total_column_ozone', 'total_column_sulphur_dioxide',
        ],
        'year': [
            '2015', '2016', '2017',
            '2018', '2019', '2020',
        ],
        'month': [
            '01', '02', '03',
            '04', '05', '06',
            '07', '08', '09',
            '10', '11', '12',
        ],
        'product_type': 'monthly_mean',
        'area': [
            35.49, 68.14, 6.74,
            97.36,
        ],
    },
    'cams_atmosphereData.nc')
