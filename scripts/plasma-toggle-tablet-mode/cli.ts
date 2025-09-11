import {parseArgs} from 'node:util'
import {homedir} from 'node:os'
import {join} from 'node:path'
import {name} from './package.json'
import {kconfig, kwinInputDevice} from './lib.ts'
import process from 'node:process'

function getXdgConfigDir(): string {
	return process.env.XDG_CONFIG_HOME || join(homedir(), '.config')
}

async function loadDevicesToToggle(): Promise<string[]> {
	const configPath = join(
		getXdgConfigDir(),
		'plasma-toggle-tablet-mode/config.json',
	)
	try {
		const {devices} = (await Bun.file(configPath).json()) as unknown
		if (
			Array.isArray(devices) &&
			devices.every((dev: unknown) => typeof dev === 'string')
		) {
			return devices
		}

		console.error('config.json file is invalid; using an empty list.')
	} catch (error) {
		console.error('Failed to load devices from config:', error)
	}

	return []
}

const devicesToToggle = await loadDevicesToToggle()

const {values, positionals} = parseArgs({
	args: Bun.argv,
	options: {
		help: {type: 'boolean', short: 'h'},
	},
	strict: true,
	allowPositionals: true,
})

function writeHelpMessage() {
	console.log(`Usage: ${name} [options] [<command>]

Commands:
  list-devices, ls      List available input devices and their system names.
  toggle                Toggle tablet mode ON or OFF (default command).

Options:
  -h, --help            Display this help message.

Description:
  Toggle tablet mode and enable/disable the devices listed in your configuration
   file (~/.config/plasma-toggle-tablet-mode/config.json).

Configured devices to toggle:
${
	devicesToToggle.length > 0
		? devicesToToggle.map((dev) => `  - ${dev}`).join('\n')
		: '  (none found in config)'
}
`)
}

if (values.help) {
	writeHelpMessage()
	process.exit(0)
}

const cliCommand = positionals[2]

await kwinInputDevice.updateDeviceMap()

const getSysName = (deviceName: string) =>
	kwinInputDevice.deviceMap.get(deviceName)

switch (cliCommand) {
	case 'list-devices':
	case 'ls': {
		for (const [name, sysName] of kwinInputDevice.deviceMap) {
			console.log(`${name} (${sysName})`)
		}

		break
	}

	case undefined:
	case 'toggle': {
		let tabletMode = await kconfig.getTabletMode()

		console.log(`${tabletMode ? 'Disabling' : 'Enabling'} tablet mode`)
		tabletMode = !tabletMode
		await kconfig.setTabletMode(tabletMode)

		for (const deviceName of devicesToToggle) {
			const sysName = getSysName(deviceName)!
			console.log(`${tabletMode ? 'Disabling' : 'Enabling'} ${deviceName}`)
			await kwinInputDevice.setDeviceStatus(sysName, !tabletMode)
		}

		break
	}

	default: {
		console.error(`Unknown command: [${cliCommand}]`)
		writeHelpMessage()
		process.exit(0)
	}
}
