<script lang="ts">
	import { slide } from 'svelte/transition';
	import '../../../theme/variables.css';
	// import Icon from '@iconify/svelte';
	import { informationCircleOutline } from 'ionicons/icons';

	let show_info = false;

	$: delay = 12;
	$: total_delay = delay + 5;
	$: lav_kv = true;
	$: kg = 70;
	$: kontrast = Math.round(lav_kv ? kg * 0.75 : kg * 0.9);
	$: {
		if (kontrast < 50) {
			kontrast = 50;
		}
		if (kontrast > 90) {
			kontrast = 90;
		}
	}
	$: saltvann = Math.round((total_delay - kontrast / 6) * 6);
	$: {
		if (saltvann < 0) {
			saltvann = 0;
		}
	}

	function radioSelect(event: { detail: any }) {
		lav_kv = event.detail.value;
	}

	function handleDelay(event: any) {
		delay = event.target.value;
	}

	function handleKg(event: any) {
		kg = event.target.value;
	}
</script>

<div
	transition:slide
	class="pointer"
	on:click={() => (show_info = !show_info)}
	on:keypress={() => (show_info = !show_info)}
	id="click-trigger"
>
	<!-- <Icon icon="ph:info-bold" /> -->
	<ion-icon icon={informationCircleOutline} color="secondary" />
</div>

<ion-popover trigger="click-trigger" trigger-action="click">
	<ion-content class="ion-padding"
		>Kalkulator for å beregne kontrastmengde og saltvann for<br />CT koronar angiografi utført med
		FLASH teknikk!<br /> (forutsetter jodholdig kontrast med konsentrasjon <br />350 mgI/ml og
		injeksjonshastighet på 6 ml/s)<br /><br />
		Gir en dose på <b>0.75 ml</b> per kg kroppsvekt for <b>70-80 kV</b>,<br />og <b>0.90 ml</b>
		per kg kroppsvekt for <b>90 kV</b> og høyere.<br /><br />Maks dose kontrast er <b>90 ml</b>, og
		minstedose er <b>50 ml</b>.</ion-content
	>
</ion-popover>

<form class="input_area">
	<ion-list>
		<ion-item class="sub_area">
			<ion-label>
				<ion-text>Vekt (kg):</ion-text>
			</ion-label>
			<ion-input name="vekt_input" type="number" value={kg} min="1" on:change={handleKg} />
			<ion-note slot="helper">Add some helper text here</ion-note>
			<ion-note slot="error">I AM AN ERROR FROM THE NUMBER INPUT!</ion-note>
		</ion-item>

		<ion-item class="sub_area">
			<ion-list>
				<ion-list-header>
					<ion-label>Rørspenning (kV):</ion-label>
				</ion-list-header>
				<ion-radio-group name="lav_kv" on:ionChange={radioSelect} value={lav_kv}>
					<ion-item>
						<ion-radio name="kv_input_a" value={true} aria-checked />
						70-80 kV
					</ion-item>
					<ion-item>
						<ion-radio name="kv_input_b" value={false} />
						90+ kV
					</ion-item>
					<ion-note slot="helper">I'm the radio button helper text!</ion-note>
				</ion-radio-group>
			</ion-list>
		</ion-item>

		<ion-item class="sub_area">
			<ion-label>
				<ion-text>Tid til max HU (sekunder):</ion-text>
			</ion-label>
			<ion-input name="delay_input" type="number" value={delay} min="12" on:change={handleDelay} />
		</ion-item>
	</ion-list>
</form>

<div class="spacer" />
<h3>
	Total delay: &emsp;&emsp;{total_delay} s
</h3>
<h3>
	Kontrast: &emsp;&emsp;{kontrast} ml
</h3>
<h3>
	Saltvann: &emsp;&emsp;{saltvann} ml
</h3>
<div class="spacer" />
<div class="spacer" />
<div class="spacer" />

<style>
	.input_area {
		display: flex;
		flex-direction: column;
		justify-content: center;
		align-content: space-between;
		width: 66vw;
		max-width: 350px;
	}
	.sub_area {
		display: flex;
		flex-direction: row;
		flex-wrap: wrap;
		align-items: center;
		justify-content: space-between;
		padding: 0.375rem;
	}
	.pointer {
		cursor: pointer;
	}
	.spacer {
		flex-grow: 1;
	}
</style>
