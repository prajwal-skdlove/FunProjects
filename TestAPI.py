import pandas as pd
d = {'State' : ['OH'], 'FICO' : [800], 'Lien': [1], 'LTV': [80], 'Term': [999], 'Margin': [2.5]}
df = pd.DataFrame(d)
df.set_index(['State', 'FICO', 'Lien', 'LTV', 'Term'], inplace = True)

#API endpoint for response variable retrieval

def get_marginbin(State, FICO, Lien, LTV, Term):
    return df.loc[(State, FICO, Lien, LTV, Term)]['Margin'].item()

#%%
   

from flask import Flask, request, jsonify



app = Flask(__name__)
app.config['JSON_SORT_KEYS'] = False

@app.route('/get_rates', methods=['POST'])
def calculate_monthly_payment():
    data = request.get_json()

    # Check if all required parameters are present
    if 'State' not in data or 'FICO' not in data or 'Lien' not in data or 'LTV' not in data:
        return jsonify({'error': 'Missing required parameters'}), 400

    # try:
    #     principal = float(data['principal'])
    #     rate = float(data['rate'])
    #     term = int(data['term'])
    # except (ValueError, TypeError):
    #     return jsonify({'error': 'Invalid parameter values'}), 400

    # if principal <= 0 or rate <= 0 or term <= 0:
    #     return jsonify({'error': 'Parameters must be positive'}), 400


    State = data.get('State')
    FICO = data.get('FICO')
    LTV = data.get('LTV')
    Lien = data.get('Lien')
    Term = data.get('Term', 999)  
    PromotionalDiscounts = float(data.get('PromotionsDiscount', 0))
    ACHDiscount = float(data.get('ACHDiscount', 0))
    PrimeRate = 8
    PrimeRateDate = '2023-05-19'
    MinRate = 2.5
    MarginRate = get_marginbin(State, FICO, Lien, LTV, Term)     
    CalculatedRate = PrimeRate + MarginRate - PromotionalDiscounts - ACHDiscount
    Rate = max(MinRate,CalculatedRate)
    
    output = {
              'PrimeRate': PrimeRate,
              'PrimeRateDate': PrimeRateDate,
              'MinimumRate': MinRate,              
              'PromotionalDiscounts': PromotionalDiscounts,
              'ACHDiscount': ACHDiscount,
              'MarginRate': MarginRate,
              'Rate': Rate
              }

     

    return jsonify(output)

if __name__ == '__main__':
    app.run()

#%%
# Call API
import requests

url = 'http://localhost:5000/get_rates'
data = {
    'State': 'OH',
    'FICO' : 800,
    'Lien' : 1,
    'LTV' : 80,    
    'PromotionalDiscounts': 0,
    'ACHDiscount' : 0
}

response = requests.post(url, json=data)
print(response.status_code, ' ', response.text)
result = response.json()
