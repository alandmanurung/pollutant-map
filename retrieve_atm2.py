import cdsapi
from datetime import date

today = str(date.today())
today_str = today + "/" + today

c = cdsapi.Client()

c.retrieve(
    'cams-global-atmospheric-composition-forecasts',
    {
        'date': "2021-12-05/2021-12-05",
        'type': 'forecast',
        'format': 'netcdf_zip',
        'time': '00:00',
        'variable': [
            'particulate_matter_10um', 'particulate_matter_2.5um', 'total_column_carbon_monoxide',
            'total_column_nitrogen_dioxide', 'total_column_ozone', 'total_column_sulphur_dioxide',
        ],
        'leadtime_hour': [
            '0', '1', '10',
            '100', '101', '102',
            '103', '104', '105',
            '106', '107', '108',
            '109', '11', '110',
            '111', '112', '113',
            '114', '115', '116',
            '117', '118', '119',
            '12', '120', '13',
            '14', '15', '16',
            '17', '18', '19',
            '2', '20', '21',
            '22', '23', '24',
            '25', '26', '27',
            '28', '29', '3',
            '30', '31', '32',
            '33', '34', '35',
            '36', '37', '38',
            '39', '4', '40',
            '41', '42', '43',
            '44', '45', '46',
            '47', '48', '49',
            '5', '50', '51',
            '52', '53', '54',
            '55', '56', '57',
            '58', '59', '6',
            '60', '61', '62',
            '63', '64', '65',
            '66', '67', '68',
            '69', '7', '70',
            '71', '72', '73',
            '74', '75', '76',
            '77', '78', '79',
            '8', '80', '81',
            '82', '83', '84',
            '85', '86', '87',
            '88', '89', '9',
            '90', '91', '92',
            '93', '94', '95',
            '96', '97', '98',
            '99',
        ],
        'area': [
            35.49, 68.14, 6.74,
            97.36,
        ],
    },
    'download.netcdf_zip')
