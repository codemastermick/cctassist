<script>
	import { home, calculator, journal } from 'ionicons/icons';

	const menuItems = [
		{
			name: 'Home',
			link: '/',
			icon: home
		},
		{
			name: 'Kontrast Kalkulator',
			link: '/tools/contrastCalculator',
			icon: calculator
		},
		{
			name: 'Calcium Score Percentile Calculator',
			link: '/tools/calciumScore',
			icon: calculator
		},
		{
			name: 'CAD-RADS',
			link: '/tools/cadrads',
			icon: calculator
		},
		{
			name: 'Standardized Report',
			link: '/tools/report',
			icon: journal
		}
	];
</script>

<ion-list>
	{#each menuItems as item}
		<ion-menu-toggle>
			<ion-item>
				<ion-icon slot="start" icon={item.icon} />
				<a href={item.link}>{item.name}</a>
			</ion-item>
		</ion-menu-toggle>
	{/each}
</ion-list>

<style>
	ion-list {
		display: flex;
		flex-direction: column;
	}
</style>
