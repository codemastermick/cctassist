# CCTAssist

<!-- TODO (@neonspork): add some kind of project description here -->

## Developing

Once you've cloned the project and installed dependencies with `npm install` (or `pnpm install` or `yarn`), start a development server:

```bash
npm run dev

# or start the server and open the app in a new browser tab
npm run dev -- --open
```

## Building

To create a production version of your app:

```bash
npm run build
npx cap sync
```

You can preview the production build with `npm run preview`.

## Translations

To add a new language to the app you can simply copy the entire `src/lib/translations/en` folder and then rename the folder to the ISO-3166 country code for the language you want to work on, then just go through all the json files and translate the value stored in each key. For example, if you were translating the common file to Norwegian:

```
# src/lib/translations/no/common.json
{
	"menu": "Meny",
	"tools": "Verktøy",
    ...
}
```
