import { For, Show } from 'solid-js';

import { useStore } from '@nanostores/solid';
import { appStore, Results, Value } from '../app';

function display(value: Value) {
    switch (value.type) {
        case 'Object': {
            const fields = value.fields!;
            let result = '{ ';
            for (const [key, value] of fields.entries()) {
                result += `${key}: ${display(value)}, `;
            }
            return result.slice(0, -2) + ' }';
        }
        case 'Int':
            return value.value;
        default:
            return "todo";
    }
}

export default function Results() {
    const $appStore = useStore(appStore);

    return <div>
        <Show when={$appStore().results}>
            <h3>Objects</h3>
            <For each={Array.from($appStore().results!.objects.entries())}>
                {([object_type, values]) =>
                    <div>
                        <h4>{object_type}</h4>
                        <table>
                            <thead>
                                <tr>
                                    <For each={Array.from(values[0]!.value.fields!.keys())}>
                                        {key => <th>{key}</th>}
                                    </For>
                                </tr>
                            </thead>
                            <tbody>
                                <For each={values}>
                                    {value => <tr>
                                        <For each={Array.from(value.value.fields!.values())}>
                                            {(value) => <td>{display(value)}</td>}
                                        </For>
                                    </tr>}
                                </For>
                            </tbody>
                        </table>
                    </div>
                }
            </For>
            <h3>Output</h3>
            <For each={Array.from($appStore().results!.output)}>
                {output => <div>{output.value}</div>}
            </For>
            <h3>Errors</h3>
        </Show>


        {/* <div>{JSON.stringify(results.objects)}<div /> */}
        {/* <div>
                <h2>Results</h2>
                <p>{JSON.stringify($appStore().results)}</p>
                <h2>Errors</h2>
                <p>{JSON.stringify($appStore().errors)}</p>
            </div> */}
    </div>
}
