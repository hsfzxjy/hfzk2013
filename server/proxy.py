import open.sina

TYPE = {'sina':1,'twitter':2,'facebook':3}

def update_expire_in(data):
    id = TYPE[data["account_type"]]
    if id == 1:
        data["expire_in"] = int(
            open.sina.Sina(data["access_token"]).get_info()["expire_in"])
