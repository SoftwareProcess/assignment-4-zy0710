import hashlib

str = '0000000000020121340000'

h = hashlib.sha256()
h.update(str.encode())
integrity = h.hexdigest()

print(type(integrity))