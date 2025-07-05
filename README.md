# 🍥 ClemScript 🤭

ClemScript est un langage de programmation simple et interactif qui permet aux utilisateurs de créer des applications, des quiz et d'effectuer des opérations de base de manière intuitive.

## Utilisation

Pour exécuter un fichier ClemScript, utilisez la commande suivante :

```bash
./ClemScript/main <filename.clem>
```
Assurez-vous que le fichier contient le code ClemScript valide.

## Fonctions Principales

**Déclaration de Variables** : Vous pouvez déclarer des variables et leur donner des valeurs initiales.

```
Clem var Script x = 10;
```
**Affichage dans la Console** : Affichez des messages ou des valeurs dans la console.

```
Clem console Script -> "La somme est : " + (x + 20);
```
**Structures Conditionnelles** : Utilisez des structures conditionnelles pour contrôler le flux d'exécution.

```
Clem if Script (x < 20) Clem then Script {
    Clem console Script -> "X est moins que 20";
}
```

**Boucles** : Créez des programmes répétitifs avec des boucles while et for.

```
Clem for Script i Clem from Script 0 Clem to Script 5 Clem do Script {
    Clem console Script -> "Index : " + i;
}
```
**Couleurs de Texte** : Personnalisez les affichages avec des couleurs (supporte plusieurs couleurs).

```
Clem color Script "red" "Ceci est un texte rouge";

```
**Création de Quiz** : Créez des quiz interactifs

```
Clem quiz Script "Quelle est la capitale de la France?" 
    Clem options Script "Paris" "Londres" "Berlin" Clem answer Script 1;
 ```
 ## Exemples de Code 
Voici un exemple de script ClemScript :

```
Clem var Script x = 10;
Clem var Script y = 20;
Clem console Script -> "La somme est : " + (x + y);
```

## installation
Aucune installation spécifique n'est requise. Il suffit de cloner ce dépôt et de suivre les instructions ci-dessus pour exécuter vos fichiers ClemScript.

**Bonne expérimentation avec mon langage de programmation** 😉