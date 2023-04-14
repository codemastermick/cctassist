import i18n from 'sveltekit-i18n';
import lang from './lang.json';

/** @type {import('sveltekit-i18n').Config} */
const config = ({
    fallbackLocale: 'en',
    translations: {
        en: { lang },
        ms: { lang }
    },
    loaders: [
        {
            locale: 'en',
            key: 'common',
            loader: async () => (
                await import('./en/common.json')
            ).default,
        },
        {
            locale: 'en',
            key: 'home',
            routes: ['/'], // you can use regexes as well!
            loader: async () => (
                await import('./en/home.json')
            ).default,
        },
        {
            locale: 'en',
            key: 'about',
            routes: ['/about'],
            loader: async () => (
                await import('./en/about.json')
            ).default,
        },
        {
            locale: 'en',
            key: 'contrastCalculator',
            routes: ['tools/contrastCalculator'],
            loader: async () => (
                await import('./en/contrast.json')
            ).default,
        },
        {
            locale: 'en',
            key: 'calciumScore',
            routes: ['tools/calciumScore'],
            loader: async () => (
                await import('./en/calcium.json')
            ).default,
        },
        {
            locale: 'en',
            key: 'cadrads',
            routes: ['tools/cadrads'],
            loader: async () => (
                await import('./en/cadrads.json')
            ).default,
        },
        {
            locale: 'en',
            key: 'report',
            routes: ['tools/report'],
            loader: async () => (
                await import('./en/report.json')
            ).default,
        }
    ],
});

export const { t, locale, locales, loading, loadTranslations } = new i18n(config);

loading.subscribe(($loading) => $loading && console.log('Loading translations...'));