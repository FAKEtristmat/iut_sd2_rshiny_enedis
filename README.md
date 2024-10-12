# iut_sd2_rshiny_enedis
Travail réalisé par **Tristan** et **Arnaud** ayant pour but de créer un site permettant de visualiser des données grâce au langage R-Shiny.

## Projet
Nous sommes l'entreprise `GreenTech Solutions`, une société de service fictive qui développe des applications. Enedis nous a sollicités en vue d’évaluer l’impact de la classe de Diagnostic de Performance Energétique (DPE) sur les consommations électriques des logements. Nous avons choisi de nous focaliser uniquement sur le département de **Loir-et-Cher**.

## Objectifs du Projet
Ce projet vise à sensibiliser sur l'importance de l'efficacité énergétique des bâtiments en analysant l’impact des classes DPE sur la consommation électrique. L'objectif est de fournir à Enedis des insights basés sur des données précises pour améliorer la gestion des consommations énergétiques à l'échelle locale.

## Données
Pour récupérer les données, nous avons utilisé deux API disponibles sur le site `Data.ademe.fr`. Ces API fournissent des informations sur les logements **existants** et **neufs** du département de Loir-et-Cher. Nous avons ensuite sélectionné les variables nécessaires à notre analyse.

### Liens API :
- [Logements Existants](https://data.ademe.fr/datasets/dpe-v2-logements-existants/api-doc)
- [Logements Neufs](https://data.ademe.fr/datasets/dpe-v2-logements-neufs/api-doc)

## Fonctionnalités de l'application
Notre application R-Shiny offre plusieurs fonctionnalités :
- **Visualisation des données** : Diagrammes et graphiques interactifs pour explorer les tendances de consommation énergétique par classe DPE, type de Logement.
- **Cartographie** : Carte interactive pour localiser les logements et visualiser des informations sur ceux-ci.
- **Filtres avancés** : Filtres permettant de segmenter les données par période de construction, type de logement.

## Technologies Utilisées
Les technologies suivantes ont été employées pour développer ce projet :
- **R** : Langage de programmation pour manipuler les données.
- **Shiny** : Framework R pour créer l'interface web interactive.
- **ggplot2** : Bibliothèque pour générer des graphiques et des visualisations.
- **leaflet** : Bibliothèque R pour la création de cartes interactives.
- **dplyr** : fonctions simples et puissantes pour filtrer, sélectionner, trier et résumer des données dans des data frames
- **shinydashboard** : créer facilement des tableaux de bord structurés et interactifs.
- **ggpubr** : offre des fonctions supplémentaires pour créer des visualisations statistiques prêtes à être publiées. Il simplifie l'ajout de thèmes, de statistiques descriptives et de légendes aux graphiques
- **DT** : permet d'intégrer facilement des tables interactives dans des applications Shiny. Il repose sur la librairie JavaScript DataTables, qui permet de trier, filtrer et rechercher dans les tableaux directement depuis l'interface utilisateur.

## Instructions pour l'installation
Pour cloner et exécuter l'application localement, suivez ces instructions :

```bash
git clone https://github.com/votre-repo/iut_sd2_rshiny_enedis.git
cd iut_sd2_rshiny_enedis
# Lancer l'application
R -e "shiny::runApp()"
