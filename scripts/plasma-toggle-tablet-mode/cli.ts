import {parseArgs} from 'node:util'
import {kconfig, kwinInputDevice} from './lib.ts'
import {name} from './package.json'

const devicesToToggle = [
	'AT Translated Set 2 keyboard',
	'ELAN06D5:00 04F3:32B7 Touchpad',
	'TPPS/2 Elan TrackPoint',
	'ThinkPad Extra Buttons',
]

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
  Toggle tablet mode and enable/disable the following devices:
    - AT Translated Set 2 keyboard
    - ELAN06D5:00 04F3:32B7 Touchpad
    - TPPS/2 Elan TrackPoint
    - ThinkPad Extra Buttons
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
