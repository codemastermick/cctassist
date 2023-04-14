import { browser } from '$app/environment';
import { writable } from 'svelte/store';
import type { Writable } from 'svelte/store';

let canWrite = false;
if (browser) {
	canWrite = localStorage.cookieAccepted;
}

let theme = writable('light');
if (browser && canWrite) {
	theme = writable(localStorage.theme);
	theme.subscribe((value) => {
		localStorage.setItem('theme', value === 'dark' ? 'dark' : 'light');
	});
}

const compactMode = writable(false);
const menuOpen = writable(false);

export { theme, menuOpen, compactMode };
