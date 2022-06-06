# Spaceship-Titanic
Predict which passengers are transported to an alternate dimension

## Descripción
Este proyecto forma parte de la asignatura *Tipología y Ciclo de vida de los Datos*, dentro del plan de estudios del Máster de Ciencia de Datos de la Universitat Oberta de Catalunya.

En él, se realiza el análisis y preparación del conjunto de datos Spaceship-Titanic previo a la realización de un modelo supervisado que permite predecir si un pasajero ha sido transportado a otra dimensión o no (objetivo de la competición de Kaggle *Spaceship-Titanic*).

## Autores
Este proyecto ha sido realizado por Juan Luis González Rodríguez y Rocío González Martínez, alumnos del Máster de Ciencia de Datos de la UOC.

## Estructura del proyecto
PDF/: Carpeta que contiene el fichero en pdf con la memoria de la práctica.
results/: directorio para guardar los CSV resultantes de las sucesivas búsquedas.
sample_dataframe/: directorio que contiene el dataset de ejemplo generado para el término de búsqueda del ibuprofeno.
src/cima/searcher.py: configura la clase CimaWebConfigurator con los parámetros y funciones de la web del medicamento.
src/cima/crawler.py: configuración de la clase Crawler, que representa el "rastreador" del medicamento por la Web de Cima.
src/cima/medicament.py: configuración de las clases WebMedicament y Medicament. La primera hace referencia a la página web en la que se describe el medicamento y la segunda, a toda la información del medicamento en sí.
src/file_csv.py: fichero que guarda el código para crear el CSV con la información recolectada en las clases anteriores.
./main.py: fichero principal del programa. En este punto se debe definir el medicamento a buscar y se obtendrá el CSV buscado.
test/test.py: fichero de pruebas y ejemplos de las clases y funciones anteriores.
