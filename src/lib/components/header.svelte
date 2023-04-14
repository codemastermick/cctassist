<script lang="ts">
	import DarkmodeToggle from './DarkmodeToggle.svelte';
	import hjertelogo from '../images/hjertelogo.svg';
</script>

<div class="header">
	<div class="subheader">
		<!-- TODO (@neonspork): this img should have a fixed width or height, maybe both -->
		<img
			class="logo logo-background"
			alt="Stylized anatomically correct heart in bright colors"
			src={hjertelogo}
		/>
		<div>
			<div class="title_row">
				<div class="spacer" />
				<h2>CT Flash kontrastkalulator</h2>
				<div class="spacer" />
				<div class="pointer darkmode">
					<DarkmodeToggle />
				</div>
			</div>
		</div>
	</div>
</div>

<style>
	.header {
		display: flex;
		flex-direction: row;
		padding: 0.5rem;
		justify-content: center;
		align-items: center;
	}

	.subheader {
		display: flex;
		flex-direction: column;
		justify-content: center;
		align-items: center;
		width: 100vw;
	}

	.title_row {
		display: flex;
		flex-direction: row;
		padding: 0.5rem;
		justify-content: center;
		align-items: center;
		width: 100vw;
	}

	.darkmode {
		padding: 0.25rem;
		/* padding-right: 0.75rem; */
		z-index: 99;
	}

	.logo {
		max-height: 5rem;
		padding: 0.375rem;
	}

	.spacer {
		flex-grow: 1;
	}
</style>
