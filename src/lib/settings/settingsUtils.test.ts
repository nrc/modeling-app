import { DeepPartial } from 'lib/types'
import { Configuration } from 'wasm-lib/kcl/bindings/Configuration'
import {
  configurationToSettingsPayload,
  getAllCurrentSettings,
  projectConfigurationToSettingsPayload,
  setSettingsAtLevel,
} from './settingsUtils'
import { createSettings } from './initialSettings'

describe(`testing settings initialization`, () => {
  it(`sets settings at the 'user' level`, () => {
    let settings = createSettings()
    const appConfiguration: DeepPartial<Configuration> = {
      settings: {
        app: {
          appearance: {
            theme: 'dark',
            color: 190,
          },
        },
      },
    }

    const appSettingsPayload = configurationToSettingsPayload(appConfiguration)

    setSettingsAtLevel(settings, 'user', appSettingsPayload)

    expect(settings.app.theme.current).toBe('dark')
    expect(settings.app.themeColor.current).toBe('190')
  })

  it(`doesn't read theme from project settings`, () => {
    let settings = createSettings()
    const appConfiguration: DeepPartial<Configuration> = {
      settings: {
        app: {
          appearance: {
            theme: 'dark',
            color: 190,
          },
        },
      },
    }
    const projectConfiguration: DeepPartial<Configuration> = {
      settings: {
        app: {
          appearance: {
            theme: 'light',
            color: 200,
          },
        },
      },
    }

    const appSettingsPayload = configurationToSettingsPayload(appConfiguration)
    const projectSettingsPayload =
      projectConfigurationToSettingsPayload(projectConfiguration)

    setSettingsAtLevel(settings, 'user', appSettingsPayload)
    setSettingsAtLevel(settings, 'project', projectSettingsPayload)

    // The 'project'-level for `theme` setting should be ignored completely
    expect(settings.app.theme.current).toBe('dark')
    // But the 'project'-level for `themeColor` setting should be applied
    expect(settings.app.themeColor.current).toBe('200')
  })
})

describe(`testing getAllCurrentSettings`, () => {
  it(`returns the correct settings`, () => {
    // Set up the settings
    let settings = createSettings()
    const appConfiguration: DeepPartial<Configuration> = {
      settings: {
        app: {
          appearance: {
            theme: 'dark',
            color: 190,
          },
        },
      },
    }
    const projectConfiguration: DeepPartial<Configuration> = {
      settings: {
        app: {
          appearance: {
            theme: 'light',
            color: 200,
          },
        },
        modeling: {
          base_unit: 'ft',
        },
      },
    }

    const appSettingsPayload = configurationToSettingsPayload(appConfiguration)
    const projectSettingsPayload =
      projectConfigurationToSettingsPayload(projectConfiguration)

    setSettingsAtLevel(settings, 'user', appSettingsPayload)
    setSettingsAtLevel(settings, 'project', projectSettingsPayload)

    // Now the test: get all the settings' current resolved values
    const allCurrentSettings = getAllCurrentSettings(settings)
    // This one gets the 'user'-level theme because it's ignored at the project level
    // (see the test "doesn't read theme from project settings")
    expect(allCurrentSettings.app.theme).toBe('dark')
    expect(allCurrentSettings.app.themeColor).toBe('200')
    expect(allCurrentSettings.modeling.defaultUnit).toBe('ft')
  })
})
