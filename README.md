
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

**Conditions Renforcées** : Support des opérateurs logiques && (ET) et || (OU).

```
Clem if Script (x > 5 && y < 25) Clem then Script {
    Clem console Script -> "Les deux conditions sont vraies";
}

Clem if Script (x < 5 || y > 15) Clem then Script {
    Clem console Script -> "Au moins une condition est vraie";
}
```

**Boucles** : Créez des programmes répétitifs avec des boucles while et for.

```
Clem for Script i Clem from Script 0 Clem to Script 5 Clem do Script {
    Clem console Script -> "Index : " + i;
}
```

**Saisie Utilisateur** : Demandez des données à l'utilisateur.

```
Clem input Script nom "Entrez votre nom: ";
Clem console Script -> "Bonjour " + nom + "!";
```

**Fonctions Mathématiques** : Effectuez des calculs mathématiques de base.

```
Clem var Script carre;
carre = Clem math Script square 5;    # 5²

Clem var Script racine;
racine = Clem math Script sqrt 16;    # √16

Clem var Script absolu;
absolu = Clem math Script abs (-8);   # |−8|
```

**Couleurs de Texte** : Personnalisez les affichages avec des couleurs (supporte plusieurs couleurs).

```
Clem color Script "red" "Ceci est un texte rouge";
Clem color Script "green" "Ceci est un texte vert";
```

**Création de Quiz** : Créez des quiz interactifs

```
Clem quiz Script "Quelle est la capitale de la France?" 
    Clem options Script "Paris" "Londres" "Berlin" Clem answer Script 1;
 ```

### Opérateurs Logiques
- `&&` : ET logique (AND)
- `||` : OU logique (OR)

### Fonctions Utiles
- `Clem input Script <variable> <prompt>` : Saisie utilisateur
- 
### Couleurs Supportées
- `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`

## Exemples de Code 

Voici un exemple de script ClemScript utilisant les nouvelles fonctionnalités :

```
Clem var Script x = 10;
Clem var Script y = 20;

# Conditions renforcées
Clem if Script (x > 5 && y < 25) Clem then Script {
    Clem console Script -> "Conditions multiples satisfaites!";
}

Clem color Script "green" "Exécution terminée avec succès!";
```
## 🍥 Nouvelles fonctionnalités 🎉

Le langage de programmation ``💚 ClemScript🩷`` possède les fonctionnalités : 

- **Clem interact Script** : fonctionne avec input, et Clem then Script Pour crée vos histoires et jeux interactifs avec choix.
  
- **Clem random valeur Script** min max : pour générer un nombre aléatoire entre un minimum et un maximum de votre choix

- **Clem random text** : pour afficher un texte aléatoire parmis les textes de votre choix.

## 🍘 Exemple d'utilisation des nouvelles fonctionnalités dans un script 

```
Clem color Script "green" "Génération d'un nombre aléatoire...";
Clem random valeur Script 1 100;

Clem color Script "blue" "Texte aléatoire:";
Clem random text Script "Bonjour", "Salut", "Coucou", "Hello", "Hi";

Clem var Script choix;
Clem input Script choix "Choisissez votre aventure (forêt/montagne/océan): ";

Clem interact Script choix "forêt", "montagne", "océan" Clem then Script {
    Clem color Script "green" "Vous avez choisi une aventure valide!";
    Clem random text Script "Vous explorez la nature", "Une grande aventure vous attend", "Le paysage est magnifique";
} Clem else Script {
    Clem color Script "red" "Choix non reconnu. Essayez forêt, montagne ou océan.";
}
```
  
## Installation
Aucune installation spécifique n'est requise. Il suffit de cloner ce dépôt et de suivre les instructions ci-dessus pour exécuter vos fichiers ClemScript.

**Bonne expérimentation avec mon langage de programmation** 😉

♥️ si vous souhaitez en voir plus, parcourez mon github pour découvrir mes autres langages de programmation et mes autres projets!♥️