


#Executing Code
# Você pode executar o código Python dentro do módulo principal usando as funções py_run_file e py_run_string. Essas funções retornam uma referência ao módulo principal do Python para que você possa acessar os resultados de sua execução. 
#Por exemplo:  
py_run_file("script.py")

main <- py_run_string("x = 10")
main$x

py_available()
py_config()


#Formato de arquivo para python e R:
#pesquisar leaf
